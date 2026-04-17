# eophis API
import eophis
from eophis import Freqs
# other modules
from math import sin, pi
import argparse
import os

def ocean_info():
    # ocean namelist
    nemo_nml = eophis.FortranNamelist(os.path.join(os.getcwd(),'namelist_cfg'))
    step,  = nemo_nml.get('rn_Dt')
    nlvl = 36

    # coupling config
    tunnel_config = list()
    tunnel_config.append( { 'label' : 'TO_NEMO_FIELDS', \
                            'grids' : { 'DINO_Grid' : {'npts' : (62,199), 'halos' : 7, 'bnd' : ('close','close') }  }, \
                            'exchs' : [ {'freq' : step, 'grd' : 'DINO_Grid', 'lvl' : 1, 'in' : ['Hu','Hv','Db_u','Db_v','Fbuoy','taum'], 'out' : ['psi_u','psi_v']},
                                        {'freq' : step, 'grd' : 'DINO_Grid', 'lvl' : nlvl, 'in' : ['en_prod','en_rhs'], 'out' : []}
                                      ] }
                        )
                        
    # static coupling (manual send/receive)
    tunnel_config.append( { 'label' : 'TO_NEMO_METRICS', \
                            'grids' : { 'DINO_Grid' : {'npts' : (62,199), 'halos' : 7, 'bnd' : ('close','close') }  }, \
                            'exchs' : [ {'freq' : Freqs.STATIC, 'grd' : 'DINO_Grid', 'lvl' : 1, 'in' : ['htau','e1u','e2v'], 'out' : []},
                                        {'freq' : Freqs.STATIC, 'grd' : 'DINO_Grid', 'lvl' : nlvl, 'in' : ['e3t','e3w','depthw'], 'out' : []}
                                      ] }
                        )
                        
    return tunnel_config, nemo_nml


def preproduction():
    eophis.info('========= MORAYS : Pre-Production =========')
    eophis.info('  Aim: write coupling namelist\n')

    # ocean info
    tunnel_config, nemo_nml = ocean_info()
    step, it_end, it_0 = nemo_nml.get('rn_Dt','nn_itend','nn_it000')
    total_time = (it_end - it_0 + 1) * step

    # tunnel registration (lazy) compulsory to update namelist
    eophis.register_tunnels( tunnel_config )
    
    # write updated namelist
    eophis.write_coupling_namelist( simulation_time=total_time )


def production():
    eophis.info('========= MORAYS : Production =========')
    eophis.info('  Aim: execute coupled simulation\n')

    #  Ocean Coupling
    # ++++++++++++++++
    tunnel_config, nemo_nml = ocean_info()
    step, it_end, it_0 = nemo_nml.get('rn_Dt','nn_itend','nn_it000')
    niter = it_end - it_0 + 1
    total_time = niter * step

    # tunnel registration (lazy)
    nemo, nemo_metrics = eophis.register_tunnels( tunnel_config )

    # link all tunnels (beware, dormant errors will likely appear here)
    eophis.open_tunnels()

    #  Models
    # ++++++++
    from models import mle_stream_func

    # get metrics
    e1u = nemo_metrics.receive('e1u')
    e2v = nemo_metrics.receive('e2v')
    e3t = nemo_metrics.receive('e3t')
    e3w = nemo_metrics.receive('e3w')
    depthw = nemo_metrics.receive('depthw')

    Ds_x = e1u
    Ds_x [ Ds_x > 111.e3 ] = 111.e3
    Ds_y = e2v
    Ds_y [ Ds_y > 111.e3 ] = 111.e3

    # constants
    omega = 7.292115083046062e-5
    Lat = nemo_nml.get('rn_lat')
    f_cori = 2.0 * omega * sin( Lat * pi / 180.)
    #C_Lfa = Ce / ( 5000.0 * 2.0 * omega * sin( Lat * pi / 180.) )
    rho0 = 1025.

    #  Assemble
    # ++++++++++
    @eophis.all_in_all_out(geo_model=nemo, step=step, niter=niter)
    def loop_core(**inputs):
        outputs = {}
        
        outputs['psi_u'] = mle_stream_func( 
                db=inputs['Db_u'], hmin=inputs['htau'], H=inputs['Hu'], S=Ds_x, dl=e1u, cori=f_cori, Fb=inputs['Fbuoy'], 
                dedt=inputs['en_rhs'], kN2=inputs['en_prod'], taum=inputs['taum'], rho0=rho0, 
                db2=inputs['Db_v'], dl2=e2v, dzt=e3t, dzw=e3w, zw=depthw 
                )
        outputs['psi_v'] = mle_stream_func( 
                db=inputs['Db_v'], hmin=inputs['htau'], H=inputs['Hv'], S=Ds_y, dl=e2v, cori=f_cori, Fb=inputs['Fbuoy'], 
                dedt=inputs['en_rhs'], kN2=inputs['en_prod'], taum=inputs['taum'], rho0=rho0, 
                db2=inputs['Db_u'], dl2=e1u, dzt=e3t, dzw=e3w, zw=depthw 
                )
        
        return outputs

    #  Run
    # +++++
    eophis.starter(loop_core)
    
# ============================ #
if __name__=='__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--exec', dest='exec', type=str, default='prod', help='Execution type: preprod or prod')
    args = parser.parse_args()

    eophis.set_mode(args.exec)

    if args.exec == 'preprod':
        preproduction()
    elif args.exec == 'prod':
        production()
    else:
        eophis.abort(f'Unknown execution mode {args.exec}, use "preprod" or "prod"')
