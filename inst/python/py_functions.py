import os as os
import shutil as shutil
import numpy as np
from PIL import Image
import pandas as pd
from scipy import ndimage

def process_image(path, binary, median_filter, kernel, root_distribution, D, DRF):
    
    #path argument is the path to an image file exported by RootPainter
    #All features are imported in pixel units
    
    if (binary==True):
      image = np.array(Image.open(path))
      #Replace values>0 by 0. Use value=1 otherwise.
      image = np.where(image>0, 0, 1)
    else:
      # L = R * 299/1000 + G * 587/1000 + B * 114/1000
      image = np.array(Image.open(path).convert('L'))
      #Replace values>0 by 1. Keep value=0 otherwise.
      image = np.where(image>0, 1, 0)
    
    #Get image height
    h = image.shape[0]
    
    #Get image width
    w = image.shape[1]
    
    #Median filtering
    if (median_filter==True):
        image = ndimage.median_filter(image, size=kernel)
    
    #Sum the number of root pixels in each row (use axis=1 for row sums)
    px = np.sum(image, axis=1)
    
    #Calculate total number of root pixels
    totalpx = np.sum(px)
    
    #Get relative root surface area
    rrsa = totalpx/(w*h)
    
    #Calculate root distribution indices
    
    if (root_distribution==True):
        
        if (totalpx>0):
        
            depth=np.array(range(0, len(px), 1))*(-1)
        
            mrd=np.sum(depth*np.array(px))/totalpx #Mean rooting depth
            maxrd=depth[np.max(np.nonzero(px))] #Max rooting depth
            d=np.min(depth[np.cumsum(px)/totalpx <= D/100]) #D value
            drf=np.sum(px[depth<=((1-h)-(DRF/100)*(1-h))])/totalpx #Deep root fraction
            
        else:
        
            mrd=0
            maxrd=0
            d=0
            drf=0

    #Create a list with results
    file=os.path.basename(path)
    
    if (root_distribution==True):
        
        results={'image':file, 'height':h, 'width':w, 'mrd':mrd, 'maxrd':maxrd, 'd':d, 'drf':drf, 'rsa':totalpx, 'rrsa': rrsa}
    
    else:
        
        results={'image':file, 'height':h, 'width':w, 'rsa':totalpx, 'rrsa': rrsa}
    
    return results
    

def copy_new_images(path_from, path_to):
    
    files = os.listdir(path_from)

    n = 0

    for file in files:
        if not os.path.exists(os.path.join(path_to, file)):
            n = n+1
            shutil.copy(os.path.join(path_from, file), path_to)
    
    return n
