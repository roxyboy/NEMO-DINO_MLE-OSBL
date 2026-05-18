"""
Contains User Inference/Analytic Models.

A model must fit the following requisites and structure :
--------------------------------------------------------
    1. must be a callable function that takes N numpy arrays as inputs
    2. /!\ returns N None for the N awaited outputs if at least one of the input is None /!\
    3. inputs may be freely formatted and transformed into what you want BUT...
    4. ...outputs must be formatted as numpy array for sending back
"""
import numpy as np

# --------- utils ---------- #
def Is_None(*inputs):
    """ Test presence of at least one None in inputs """
    return any(item is None for item in inputs)

# ============================ #
#             MLE              #
# ============================ #
def boundary_layer_depth(dedt,kN2,dbx,dby,hmin,H,S,cori,Fsr,Fns,taum,rho0,alpha,dxu,dyv,dzw,zw,Cf):
    """
       Iteratively find the boundary layer depth to be used in MLE param.
    """
    if Is_None(dbx,dby,H):
        return None
    else:
        b_x = dbx / dxu
        b_y = dby / dyv
        mstar = 0.5
        nstar = 0.066
        # Cf = 0.03
        # rho0 = 1025.
        ustar = np.sqrt( np.abs(taum)/rho0 )
        # print(H.shape, ustar.shape, kN2.shape)
        mle = ( Cf * S * np.abs(cori) * H**2 * (b_x**2+b_y**2)
        #        / star2      # star2 is not defined at this stage
              ) * 63/44

        grav = 9.807
        # Qs = g*(beta*1e3)*dsf_sub.saltflx # m/s2.(kg/s)/m2
        Qt = (Fsr + Fns)        # W/m2 = (kg.m2/s3)/m2; positive: heat gained by the ocean
        Cp = 1005.
        rho_air = 1.225
        Ba = grav*alpha * (Qt/rho_air/Cp)
        Fb = np.abs( np.where(Ba<0., Ba, 0.) )

        bld = np.zeros_like(H)
        Nn = dedt.shape

        for i in range(Nn[0]):
            for j in range(Nn[1]):

                # rKmg = 0.7
                # kN2 = Avt[i,j] * n2[i,j]
                # diss = (0.5*rKmg) * dissl[i,j] * en[i,j]   # dissl = sqrt(en)/L
                # difu = (-np.diff( en[i,j] ) / dzt[i,j,:-1]
                #        * ..5*(Avm[i,j,1:]+Avm[i,j,:-1]))  # approximate with explicity eddy diffusion
                # difu = -np.diff( np.padd(difu, (1,1), mode="edge") 
                #               ) / dzw[i,j]  # ad-hoc Neumann boundary condition
                # dedt = shp[i,j] - kN2 + difu + diss

                # kN2 = avt[i,j] * n2[i,j]

                if np.abs(H[i,j]) > 0.:
                    
                    for k in range(1,Nn[2]+1):

                        h = np.sum( dzw[i,j,:k] )
                    
                        # if h >= hmin[i,j]:
                        mu = np.maximum(np.array([0.,]),
                                ( (1 - (2*np.abs(zw[i,j,:k])/np.abs(H[i,j]) + 1)**2)
                                  * (1 + 5/21*(2*np.abs(zw[i,j,:k])/np.abs(H[i,j]) + 1)**2) )
                                       )

                        if h < np.abs(H[i,j]):
                            wstar3 = Fb[i,j] * h    # h needs to be found iteratively!
                            # wstar3 = np.where( wstar3>0., wstar3, 0. )
                            star2 = ( mstar*ustar[i,j]**3 + nstar*wstar3 )**(2/3)
                            res = ( np.sum( np.minimum(np.array([0.,]), -kN2[i,j,:k]) * dzw[i,j,:k] )
                                + np.sum( dedt[i,j,:k] * dzw[i,j,:k] )
                                - mstar*ustar[i,j]**3
                                + nstar*np.sum( np.minimum(np.array([0.,]), kN2[i,j,:k]) * dzw[i,j,:k] )
                                + np.sum( (mle[i,j] / star2) 
                                         * h * mu[:k] * dzw[i,j,:k] 
                                        )
                                  )
                            if 'res0' not in locals():
                                res0 = np.abs(res)
                                res1 = res0
                                # print(res0.shape,res1.shape,h)
                                bld[i,j] = h
                            else:
                                if np.abs(res) < res1:
                                    res1 = np.abs(res)
                                    bld[i,j] = h
                        else:
                            break

                    if res1 < res0:
                        None
                    else:
                        bld[i,j] = np.abs(H[i,j])
                        break
                    # if 'res0' in locals():
                    del res0, res1

                else:
                    bld[i,j] = 0.
         
        return bld, star2


def mle_stream_func(db,hmin,H,S,dl,cori,Fsr,Fns,dedt,kN2,taum,rho0,db2,dl2,alpha,dzw,zw,Cf=0.03):
    """ 
        Compute vertical buoyancy flux induced streamfunction with 
        expression (27) from doi.org/10.1175%2Fjpo-d-21-0297.1 
    """
    if Is_None(db,H):
        return None
    else:
        grad_b = db / dl
        # mstar = 0.5
        # nstar = 0.066
        # ustar3 = np.sqrt(ustar2)**3
        # wstar3 = Fb*h
        # wstar3 = np.where(wstar3>0., wstar3, 0.)
        h, s2 = boundary_layer_depth(dedt,kN2,db,db2,hmin,H,S,cori,Fsr,Fns,taum,rho0,alpha,dl,dl2,dzw,zw,Cf)

        return ( Cf * S * np.abs(cori) * h * H**2 * grad_b 
                / s2 )

