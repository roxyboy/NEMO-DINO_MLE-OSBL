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
def bounday_layer_depth(shp,Avt,n2.Avm,dissl,en,dbx,dby,H,S,cori,taum,rho0,Fb,dx,dy,dzt,dzwi,zw):
    if Is_None(k):
        return None
    else:
        b_x = dbx / dx
        b_y = dby / dy
        mstar = 0.5
        nstar = 0.066
        C_f = 0.03
        # rho0 = 1026.
        ustar = np.sqrt(taum/rho0)
        wstar3 = Fb*h
        wstar3 = np.where( wstar3>0., wstar3, 0. )
        mle = ( C_f * S * np.abs(cori) * H**2 * (b_x**2+b_y**2)
               / (mstar*ustar**3 + nstar*wstar3)**(2/3) 
              ) * 63/44

        bld = np.zeros_like(H)
        N = e_t.shape
        for i in range(N[0]):
            for j in range(N[1]):

                rKmg = 0.7
                kN2 = Avt[i,j] * n2[i,j]
                diss = (0.5*rKmg) * dissl[i,j] * en[i,j]   # dissl = sqrt(en)/L
                difu = (-np.diff( en[i,j] ) / dzt[i,j,:-1]
                        * ..5*(Avm[i,j,1:]+Avm[i,j,:-1]))  # approximate with explicity eddy diffusion
                difu = -np.diff( np.padd(difu, (1,1), mode="edge") 
                               ) / dzw[i,j]  # ad-hoc Neumann boundary condition
                e_t = shp[i,j] - kN2 + difu + diss

                mu = np.maximum(np.array([0.,]), 
                                ( (1 - (2*zw[i,j]/H+1)**2)
                                 * (1 + 5/21*(2*zw[i,j]/H+1)**2) )
                               )

                for k in range(1,N[2]):
                    h = np.sum( dzw[i,j,:k] )
                    if h < H[i,j]:
                        res = ( np.sum( np.maximum(np.array([0.,]), kN2[i,j,:k]) * dzw[i,j,:k] )
                                - np.sum( e_t[i,j,:k] * dzw[i,j,:k] )
                                + mstar*ustar**3
                                - nstar*np.sum( np.minimum(np.array([0.,]), kN2[i,j,:k]) * dzw[i,j,:k] )
                                - np.sum( mle[i,j] * h * mu[:k] * dzw[i,j,:k] )
                              )
                        if k == 1:
                            res0 = np.abs(res)
                        else:
                            if np.abs(res) < res0:
                                res0 = np.abs(res)
                            else:
                                bld[i,j] = h - dzw[i,j,k-1]
                                break
                    else:
                        bld[i,j] = H[i,j]
                        break
         
        return bld


def vert_buoyancy_flux(db,h,H,S,dl,cori,Fb):
    """ Compute vertical buoyancy flux induced streamfunction with expression (13) from doi.org/10.1016/j.ocemod.2020.101678 """
    if Is_None(db,H):
        return None
    else:
        grad_b = db / dl
        mstar = 0.5
        nstar = 0.066
        C_f = 0.03
        ustar3 = np.sqrt(ustar2)**3
        wstar3 = Fb*h
        wstar3 = np.where(wstar3>0., wstar3, 0.)
        return ( C_f * S * np.abs(cori) * h * H**2 * grad_b 
                / (mstar*ustar3 + nstar*wstar3)**(2/3) )

