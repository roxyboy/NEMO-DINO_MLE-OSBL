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

# ================================================= #
#             MLE (Zhang et al., 2025)              #
# ================================================= #
def strain(u, v, mask_u, mask_v, dxu, dyu, dxv, dyv):
    if Is_None([u, v]):
        return None
    else:


def stream_function(db,H,S,dl,f,B=4,C=8):
    """ Compute stream function for vertical derivative of vertical tracer flux with expression (7) from doi.org/10.1016/j.ocemod.2025.102655 """
    if Is_None(db,H):
        return None
    else:
        grad_b = db / dl
        cori = np.maximum([f, 7e-6])
        
        return C * B * S * H**2 * cori**-2 * grad_b
 
