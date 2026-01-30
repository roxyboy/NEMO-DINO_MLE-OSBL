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
def vert_buoyancy_flux(db,H,h,S,dl,cori,ustar2,Fb):
    """ Compute vertical buoyancy flux induced streamfunction with expression (13) from doi.org/10.1016/j.ocemod.2020.101678 """
    if Is_None(db,H):
        return None
    else:
        grad_b = db / dl
        mstar = .5
        nstar = .066
        C_f = 0.01
        ustar3 = np.sqrt(ustar2)**3
        wstar3 = Fb*h
        wstar3 = np.where(wstar3>0., wstar3, 0.)
        return ( C_f * S * np.abs(cori) * h * H**2 * grad_b 
                / (mstar*ustar3 + nstar*wstar3)**(2/3) )

