# BASE CODE AND IDEA: Elon Gaffin-Cahn (December 2013). 
# ADDED: dot_diameter can be a vector i.e. to paint dots with different 
#        sizes. Santiago Alonso-Diaz (Aug. 2016) 



#FUNCTIONS ####
pkgload = function(p) {
  #Checks if package is installed.Once installed it loads the library  
  #p: package, string
  ifelse(require(package = p, character.only = T, quietly = T), 
         library(package = p, character.only = T, quietly = T), 
         c(install.packages(pkgs = p), 
           library(package = p, character.only = T, quietly = T))) 
}
pkgload('pracma') #has meshgrid
pkgload('plotrix') #has draw.circle
pkgload('tripack') #has circles

MakeCircle = function(diameter,center,bounds){
  # Arguments:
  #   diameter: of circle or dot. Circle refers to the big circle  where dots
  #             are placed 
  #   center: (x,y) coordinates of circle or dot
  #   bounds: width and height of allowed bounds for dots to appear in. 
  #           It has to be larger or equal than diameter.
  
  # For a [bounds x bounds] matrix, pixel2origin(n,m,:) is the (x,y) vector
  # from the origin (0,0) to (n,m). Origin refers to the top left corner of
  # the screen, and flipped Y coordinates as in PTB (also, all coordinates are
  # positive due to meshgrid call) i.e. this is just all the possible
  # coordinates/pixel combinations in the  box defined by bounds.
  temp = meshgrid(1:bounds[2],1:bounds[1])
  pixel2origin = list()
  pixel2origin[[1]] = temp[[2]]
  pixel2origin[[2]] = temp[[1]]
  
  # origin2center is the (x,y) vector from the origin [0,0] to the circle
  # center. Minus sign to compute vector substraction i.e. the resulting
  # vector from the substraction connects the two vectors (see below)
  origin2center = -center 
  
  # scroll through x, then y
  center2pixel = list()
  for (i in 1:2) {
    # center2pixel is the (x,y) vector from the center of the dot or circle
    # to the current pixel at (m,n) 
    center2pixel[[i]] = pixel2origin[[i]] + origin2center[i]; ##ok<AGROW>
  }
  
  # convert vector to magnitude only
  absolute_distance = sqrt(center2pixel[[1]]^2 + center2pixel[[2]]^2)  
  
  # circle is a logical array where TRUEs are where the distances are less
  # than the radius. 
  circle = absolute_distance <= diameter/2 #NOTE: you could change this for other shapes
  
  circle
}
isscalar <- function(x) is.atomic(x) && length(x) == 1L
ind2sub = function(m, ind) {
  #converts linear index of a matrix to the equivalent row and columns in an mXn matrix
  #m: number of rows of matrix
  #ind: linear index
  
  roww = ((ind-1) %% m) + 1 #Row
  coll = floor((ind-1) / m) + 1 #column
  c(roww,coll)
}

MakeDotArray = function(side,number_dots,dot_diameter,dist) {
  # INPUTS ####
  # Input arguments:
  #   SIDE:  diameter of allowed circle where the dots can appear (in pixels)
  #   NUMBER_DOTS: number of dots in the array
  #   DOT_DIAMETER: diameter in pixels of each dot. It could be a scalar or a
  #       row vector. If all dots have equal size, use a scalar is much faster.
  #       If vector, then its length has to be equal to  number_dots i.e. 
  #       for each dot please specify a diameter.
  #   DIST: scalar (zero and pos int) with minimum distance between dots (in pixels). 
  #       If zero, dots could touch but never overlap
  #   COLLOR: scalar (all dots same color) or vector with color of each dot. If vector it has to be the same size as number_dots
  #   FILENAME: Name of file to save image of dots
  # 
  # Output argument:
  #   DOT_CENTERS [NUMBER_DOTS x 2] matrix of (x,y) pixel coordinates of the
  #       centers of the dots
  #   DD: dot diameters for each of the elements in DOT_CENTERS
  
  
  # side = 400
  # number_dots = 11
  # dot_diameter = matrix(c(15), nrow = 1)
  # dist = 5

  
  
  
  # IMAGE GENERATION ####
  if(number_dots>0){
    bounds = c(round(side), round(side)) # height and width of allowed area circle
    area_center = bounds/2 # x,y coordinates of center of allowed area circle
    check = dim(dot_diameter)
    if (length(check)>0 && check[1] != 1) {
      stop('dot_diameter is not a row vector')
    }
    
    dd = sort(dot_diameter,2, decreasing = T); #to position big diameters first
    
    # initialize the circle where the dots area allowed (in LOGICAL format)
    allowed_area = MakeCircle(side - (dd[1] + dist)/2, area_center, bounds)
    # allowed_area_o=allowed_area; #for visualization
    dot_centers = matrix(ncol = 2, nrow = number_dots)
    for (i in 1:number_dots) {
      if (isscalar(dd)) {  #same dot size
        if (all(!allowed_area)) {
          stop('Density of the dot array too high!')
        }
        
        # get the (x,y) center of the current dot (choose randomly)
        if (side>round(dd+dist)) {
          #center = find(mnrnd(1, allowed_area(:)/sum(allowed_area(:)))); #This generates a linear index (scalar). 
          aa_col=matrix(allowed_area, ncol = 1)
          center = which(rmultinom(1,1, aa_col/sum(aa_col)) != 0) #This generates a linear index (scalar)
          center = ind2sub(dim(allowed_area)[1],center) # convert the linear index to center coordinates (x,y)
          
        } else if (side == round(dd+dist)){
          center = area_center
        } else {
          stop('Density of the dot array too high!')
        }
        
        # further restrict the allowed area
        allowed_area = allowed_area & !MakeCircle(2*(dd+dist),center,bounds) #2*diameter to make sure dots won't overlap
        
        # add to growing list of the (x,y) centers of the dots
        dot_centers[i,] = center
        
        
        
      } else { #different dot sizes
        if (all(!allowed_area)) {
          stop('Density of the dot array too high!')
        }
        
        
        # get the (x,y) center of the current dot (choose randomly)
        aa_col=matrix(allowed_area, ncol = 1)
        center = which(rmultinom(1,1, aa_col/sum(aa_col)) != 0)
        
        # convert the linear index to center coordinates (x,y)
        center = ind2sub(dim(allowed_area)[1],center)
        
        # add to growing list of the (x,y) centers of the dots
        dot_centers[i,] = center
        
        if (i<number_dots){
          #with different dot diameters, allowed_area has to be
          #restricted from scratch to place the next dot i.e. the 'halos'
          #due to the next diameter are different from the previous dot
          #(see make circle below).
          allowed_area = MakeCircle(side - (dd[i+1] + dist)/2, area_center, bounds)
          
          
          for (j in 1:i){
            centerT = dot_centers[j,]
            diameterT = dd[j] + dd[i+1] + 2*dist #redraws all previous dots with new 'halo', based on the diameter of the next dot to place
            allowed_area = allowed_area & !MakeCircle(diameterT,centerT,bounds);
            
          }
          
        }
        
      }
    }
    
    
    if (isscalar(dd)) {
      dd = dd*ones(number_dots,1)
    }
    
    list(centers = dot_centers, diameters = dd, bounds = bounds, area_center = area_center) #output
  } else {
    bounds = c(round(side), round(side)) # height and width of allowed area circle
    area_center = bounds/2
    list(centers = 99999, diameters = 99999, bounds = bounds, area_center = area_center) #output
  }
  
}


#Generate image files ####
#fd = "/Users/santiagoalonsodiaz/Desktop/jspsychtutorial/img/" #File directory
fd = "/Users/santiagoalonsodiaz/Desktop/Temp/" #File directory
ntrials_ratio = 1 #number of trials PER RATIO
ratios = c(0, 0.25, 0.5, 0.75, 0.9, 1)
circle_points = 100 #Number of points for the circle-drawing function
sep = 150 #separation between cloud of dots in pixels
#Dot parameters
DOTS.max = 25; #max number of dots
DOTS.min = 1;
DOTS.diam_px = 30 #(pixels)
DOTS.side_px = 400 #(pixels)
DOTS.dist = 5; #min. separation between dots (pixels) (input for MakeDotArray);
DOTS.color = 'black' #fill color of dots
#The next loop generates the numerical values to be presented and puts them in the matrix NUM:
#Col 1: smaller number
#Col 2: larger number
#Col 3: side on screen where larger number is presented (0 left, 1 right)
#Col 4: ratio between smaller/larger number
for (i in 1:length(ratios)){ 
  small = (1:DOTS.max)*ratios[i] #smaller number
  indexT = small%%1 == 0 & small >= DOTS.min #index for integers in small
  if (ratios[i] == 0) {
    indexT = !logical(DOTS.max)
  }
  
  
  small = small[indexT]
  large = 1:DOTS.max #larger number
  large = large[indexT]
  infoT = matrix(NA, nrow = ntrials_ratio, ncol = 4)
  for (j in 1:ntrials_ratio){
    indexT = sample.int(length(small),size=1,replace = T) #random selection of numerical values
    infoT[j,1] = small[indexT]
    infoT[j,2] = large[indexT]
    infoT[j,3] = sample.int(2,size=1) - 1;
    infoT[j,4] = ratios[i];
  }
  
  if (i == 1){
    NUM = infoT
  } else{
    NUM = rbind(NUM, infoT)
  }
  
}
colnames(NUM) = c('small','large','side','ratio')

#Equal dot size
for (i in 1:dim(NUM)[1]) {
  if  (i%%ntrials_ratio == 1 || ntrials_ratio == 1){
    fd_sub = paste(fd, NUM[i,'ratio']*1000,'/', sep='')
    dir.create(fd_sub)
    fd_sub = paste(fd_sub,'equaldotsize/',sep='')
    dir.create(fd_sub)
    fd_subL = paste(fd_sub,'L/',sep='')
    dir.create(fd_subL)
    fd_subR = paste(fd_sub,'R/',sep='')
    dir.create(fd_subR)
  }
  if (NUM[i,'side'] == 0){ #large number on the left
    fd_sub = fd_subL
  } else { #large number on the right
    fd_sub = fd_subR
  }
  
  if (NUM[i,'side'] == 0){ #large number on the left
    fn = paste(fd_sub, 
               paste(paste(i%%ntrials_ratio+1, #image number (DIFFERENT FROM ORDER IN NUM)
                              'Ratio', NUM[i,'ratio']*1000, #by 1000 to avoid dots in namefile
                              'L', NUM[i,'large'],
                              'R', NUM[i,'small'],
                              sep="_"),'.png',sep=""),sep="") #filename
  } else {#large number on the right
    fn = paste(fd_sub, 
               paste(paste(i%%ntrials_ratio+1, #image number (DIFFERENT FROM ORDER IN NUM)
                              'Ratio', NUM[i,'ratio']*1000, #by 1000 to avoid dots in namefile
                              'L', NUM[i,'small'],
                              'R', NUM[i,'large'],
                              sep="_"),'.png',sep=""),sep="") #filename
  }
  
  
  if (NUM[i,'side'] == 0){ #large number on the left
    LEFT = MakeDotArray(DOTS.side_px,
                        NUM[i,'large'],
                        DOTS.diam_px, 
                        DOTS.dist)
    RIGHT = MakeDotArray(DOTS.side_px,
                        NUM[i,'small'],
                        DOTS.diam_px, 
                        DOTS.dist)
  } else { #large number on the right
    LEFT = MakeDotArray(DOTS.side_px,
                        NUM[i,'small'],
                        DOTS.diam_px, 
                        DOTS.dist)
    RIGHT = MakeDotArray(DOTS.side_px,
                        NUM[i,'large'],
                        DOTS.diam_px, 
                        DOTS.dist)
  }
  
  
  png(fn, 
      width=(LEFT$bounds[1] + RIGHT$bounds[1])*1.1, 
      height=LEFT$bounds[2]*1.05, 
      res=120)
  par(mai = c(0,0,0,0), mfrow = c(1,1),
      xaxs = 'i', yaxs = 'i')
  # par(mai = c(1,1,1,1), xpd = F)
  plot(0,0, type = 'l', bty = 'n',
       ylab= '', xlab = '', #asp = 1,
       axes = F,
       xlim = c(-(DOTS.side_px+1.1*sep), 1.1*DOTS.side_px),
       ylim = c( -10, 1.05*DOTS.side_px))
  if(sum(LEFT$centers == 99999)==1){
    a = 'L' #a place holder
  } else {
    for (j in 1:dim(LEFT$centers)[1]) { #LEFT
      xL = LEFT$centers[j,1] - LEFT$bounds[1] - sep
      yL = LEFT$centers[j,2]
      rL = LEFT$diameters[j]/2
      xL_circum = seq(xL-rL,xL+rL, length.out = circle_points)
      temp = ifelse(rL^2 - (xL_circum-xL)^2<0,0,sqrt(rL^2 - (xL_circum-xL)^2))
      yL_circumA = temp + yL #above part
      temp = ifelse(rL^2 - (xL_circum-xL)^2<0,0,-sqrt(rL^2 - (xL_circum-xL)^2))
      yL_circumB = temp + yL #below part
      polygon(c(xL_circum,xL_circum[circle_points:1]),c(yL_circumA,yL_circumB[circle_points:1]),
              col = DOTS.color)
    }
  }
  
  if(sum(RIGHT$centers == 99999)==1){
    a = 'R' #a place holder
  } else {
    for (j in 1:dim(RIGHT$centers)[1]){
      xR = RIGHT$centers[j,1]
      yR = RIGHT$centers[j,2]
      rR = RIGHT$diameters[j]/2
      xR_circum = seq(xR-rR,xR+rR, length.out = circle_points)
      temp = ifelse(rR^2 - (xR_circum-xR)^2<0,0,sqrt(rR^2 - (xR_circum-xR)^2))
      yR_circumA = temp + yR #above part
      temp = ifelse(rR^2 - (xR_circum-xR)^2<0,0,-sqrt(rR^2 - (xR_circum-xR)^2))
      yR_circumB = temp + yR #below part
      polygon(c(xR_circum,xR_circum[circle_points:1]),c(yR_circumA,yR_circumB[circle_points:1]),
              col = DOTS.color)
      #circles(x,y,r)
    }
  }
  dev.off()
  
}



#Random dot size
rangedotsize = c(10,50)
for (i in 1:dim(NUM)[1]) {
  if (i%%ntrials_ratio == 1 || ntrials_ratio == 1){
    fd_sub = paste(fd, NUM[i,'ratio']*1000,'/', sep='')
    dir.create(fd_sub)
    fd_sub = paste(fd_sub,'randomdotsize/',sep='')
    dir.create(fd_sub)
    fd_subL = paste(fd_sub,'L/',sep='')
    dir.create(fd_subL)
    fd_subR = paste(fd_sub,'R/',sep='')
    dir.create(fd_subR)
  }
  if (NUM[i,'side'] == 0){ #large number on the left
    fd_sub = fd_subL
  } else { #large number on the right
    fd_sub = fd_subR
  }
  
  if (NUM[i,'side'] == 0){ #large number on the left
    fn = paste(fd_sub, 
               paste(paste(i%%ntrials_ratio+1, #image number (DIFFERENT FROM ORDER IN NUM)
                           'Ratio', NUM[i,'ratio']*1000, #by 1000 to avoid dots in namefile
                           'L', NUM[i,'large'],
                           'R', NUM[i,'small'],
                           sep="_"),'.png',sep=""),sep="") #filename
  } else {#large number on the right
    fn = paste(fd_sub, 
               paste(paste(i%%ntrials_ratio+1, #image number (DIFFERENT FROM ORDER IN NUM)
                           'Ratio', NUM[i,'ratio']*1000, #by 1000 to avoid dots in namefile
                           'L', NUM[i,'small'],
                           'R', NUM[i,'large'],
                           sep="_"),'.png',sep=""),sep="") #filename
  }
  
  
  
  if (NUM[i,'side'] == 0){ #large number on the left
    DOTS.diam_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'large'],replace = T) #(pixels)
    LEFT = MakeDotArray(DOTS.side_px,
                        NUM[i,'large'],
                        DOTS.diam_px, 
                        DOTS.dist)
    DOTS.diam_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'small'],replace = T) #(pixels)
    RIGHT = MakeDotArray(DOTS.side_px,
                         NUM[i,'small'],
                         DOTS.diam_px, 
                         DOTS.dist)
  } else { #large number on the right
    DOTS.diam_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'small'],replace = T) #(pixels)
    LEFT = MakeDotArray(DOTS.side_px,
                        NUM[i,'small'],
                        DOTS.diam_px, 
                        DOTS.dist)
    DOTS.diam_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'large'],replace = T) #(pixels)
    RIGHT = MakeDotArray(DOTS.side_px,
                         NUM[i,'large'],
                         DOTS.diam_px, 
                         DOTS.dist)
  }
  
  
  png(fn, 
      width=(LEFT$bounds[1] + RIGHT$bounds[1])*1.1, 
      height=LEFT$bounds[2]*1.05, 
      res=120)
  par(mai = c(0,0,0,0), mfrow = c(1,1),
      xaxs = 'i', yaxs = 'i')
  # par(mai = c(1,1,1,1), xpd = F)
  plot(0,0, type = 'l', bty = 'n',
       ylab= '', xlab = '', #asp = 1,
       axes = F,
       xlim = c(-(DOTS.side_px+1.1*sep), 1.1*DOTS.side_px),
       ylim = c( -10, 1.05*DOTS.side_px))
  if(sum(LEFT$centers == 99999)==1){
    a = 'L' #a place holder
  } else {
    for (j in 1:dim(LEFT$centers)[1]) { #LEFT
      xL = LEFT$centers[j,1] - LEFT$bounds[1] - sep
      yL = LEFT$centers[j,2]
      rL = LEFT$diameters[j]/2
      xL_circum = seq(xL-rL,xL+rL, length.out = circle_points)
      temp = ifelse(rL^2 - (xL_circum-xL)^2<0,0,sqrt(rL^2 - (xL_circum-xL)^2))
      yL_circumA = temp + yL #above part
      temp = ifelse(rL^2 - (xL_circum-xL)^2<0,0,-sqrt(rL^2 - (xL_circum-xL)^2))
      yL_circumB = temp + yL #below part
      polygon(c(xL_circum,xL_circum[circle_points:1]),c(yL_circumA,yL_circumB[circle_points:1]),
              col = DOTS.color)
    }
  }
  
  if(sum(RIGHT$centers == 99999)==1){
    a = 'R' #a place holder
  } else {
    for (j in 1:dim(RIGHT$centers)[1]){
      xR = RIGHT$centers[j,1]
      yR = RIGHT$centers[j,2]
      rR = RIGHT$diameters[j]/2
      xR_circum = seq(xR-rR,xR+rR, length.out = circle_points)
      temp = ifelse(rR^2 - (xR_circum-xR)^2<0,0,sqrt(rR^2 - (xR_circum-xR)^2))
      yR_circumA = temp + yR #above part
      temp = ifelse(rR^2 - (xR_circum-xR)^2<0,0,-sqrt(rR^2 - (xR_circum-xR)^2))
      yR_circumB = temp + yR #below part
      polygon(c(xR_circum,xR_circum[circle_points:1]),c(yR_circumA,yR_circumB[circle_points:1]),
              col = DOTS.color)
      #circles(x,y,r)
    }
  }
  dev.off()
  
}



#Random dot size equal area
rangedotsize = c(10,50)
for (i in 1:dim(NUM)[1]) {
  if (i%%ntrials_ratio == 1 || ntrials_ratio == 1){
    fd_sub = paste(fd, NUM[i,'ratio']*1000,'/', sep='')
    dir.create(fd_sub)
    fd_sub = paste(fd_sub,'randomdotsizeEA/',sep='')
    dir.create(fd_sub)
    fd_subL = paste(fd_sub,'L/',sep='')
    dir.create(fd_subL)
    fd_subR = paste(fd_sub,'R/',sep='')
    dir.create(fd_subR)
    
  }
  if (NUM[i,'side'] == 0){ #large number on the left
    fd_sub = fd_subL
  } else { #large number on the right
    fd_sub = fd_subR
  }
  
  if (NUM[i,'side'] == 0){ #large number on the left
    fn = paste(fd_sub, 
               paste(paste(i%%ntrials_ratio+1, #image number (DIFFERENT FROM ORDER IN NUM)
                           'Ratio', NUM[i,'ratio']*1000, #by 1000 to avoid dots in namefile
                           'L', NUM[i,'large'],
                           'R', NUM[i,'small'],
                           sep="_"),'.png',sep=""),sep="") #filename
  } else {#large number on the right
    fn = paste(fd_sub, 
               paste(paste(i%%ntrials_ratio+1, #image number (DIFFERENT FROM ORDER IN NUM)
                           'Ratio', NUM[i,'ratio']*1000, #by 1000 to avoid dots in namefile
                           'L', NUM[i,'small'],
                           'R', NUM[i,'large'],
                           sep="_"),'.png',sep=""),sep="") #filename
  }
  
  
  
  if (NUM[i,'side'] == 0){ #large number on the left
    DOTS.diamLarge_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'large'],replace = T) #(pixels)
    DOTS.cumarealarge_px = sum(pi*(DOTS.diamLarge_px/2)^2)
    DOTS.diamSmall_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'small'],replace = T) #(pixels)
    DOTS.cumareasmall_px = sum(pi*(DOTS.diamSmall_px/2)^2)
    scaling = sqrt(DOTS.cumarealarge_px/pi)/sqrt(DOTS.cumareasmall_px/pi)
    DOTS.diamSmall_px = scaling*DOTS.diamSmall_px
    DOTS.cumareasmall_px = sum(pi*(DOTS.diamSmall_px/2)^2)
    if (abs(DOTS.cumareasmall_px-DOTS.cumarealarge_px)>0.001 & NUM[i,'small']>0) {
      stop('areas are not equal')
    }
    LEFT = MakeDotArray(DOTS.side_px,
                        NUM[i,'large'],
                        DOTS.diamLarge_px, 
                        DOTS.dist)

    RIGHT = MakeDotArray(DOTS.side_px,
                         NUM[i,'small'],
                         DOTS.diamSmall_px, 
                         DOTS.dist)
    
  } else { #large number on the right
    DOTS.diamLarge_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'large'],replace = T) #(pixels)
    DOTS.cumarealarge_px = sum(pi*(DOTS.diamLarge_px/2)^2)
    DOTS.diamSmall_px = sample(rangedotsize[1]:rangedotsize[2],NUM[i,'small'],replace = T) #(pixels)
    DOTS.cumareasmall_px = sum(pi*(DOTS.diamSmall_px/2)^2)
    scaling = sqrt(DOTS.cumarealarge_px/pi)/sqrt(DOTS.cumareasmall_px/pi)
    DOTS.diamSmall_px = scaling*DOTS.diamSmall_px
    DOTS.cumareasmall_px = sum(pi*(DOTS.diamSmall_px/2)^2)
    if (abs(DOTS.cumareasmall_px-DOTS.cumarealarge_px)>0.001 & NUM[i,'small']>0) {
      stop('areas are not equal')
    }
    LEFT = MakeDotArray(DOTS.side_px,
                        NUM[i,'small'],
                        DOTS.diamSmall_px, 
                        DOTS.dist)
    
    RIGHT = MakeDotArray(DOTS.side_px,
                         NUM[i,'large'],
                         DOTS.diamLarge_px, 
                         DOTS.dist)
    
    
  }
  
  
  png(fn, 
      width=(LEFT$bounds[1] + RIGHT$bounds[1])*1.1, 
      height=LEFT$bounds[2]*1.05, 
      res=120)
  par(mai = c(0,0,0,0), mfrow = c(1,1),
      xaxs = 'i', yaxs = 'i')
  # par(mai = c(1,1,1,1), xpd = F)
  plot(0,0, type = 'l', bty = 'n',
       ylab= '', xlab = '', #asp = 1,
       axes = F,
       xlim = c(-(DOTS.side_px+1.1*sep), 1.1*DOTS.side_px),
       ylim = c( -10, 1.05*DOTS.side_px))
  if(sum(LEFT$centers == 99999)==1){
    a = 'L' #a place holder
  } else {
    for (j in 1:dim(LEFT$centers)[1]) { #LEFT
      xL = LEFT$centers[j,1] - LEFT$bounds[1] - sep
      yL = LEFT$centers[j,2]
      rL = LEFT$diameters[j]/2
      xL_circum = seq(xL-rL,xL+rL, length.out = circle_points)
      temp = ifelse(rL^2 - (xL_circum-xL)^2<0,0,sqrt(rL^2 - (xL_circum-xL)^2))
      yL_circumA = temp + yL #above part
      temp = ifelse(rL^2 - (xL_circum-xL)^2<0,0,-sqrt(rL^2 - (xL_circum-xL)^2))
      yL_circumB = temp + yL #below part
      polygon(c(xL_circum,xL_circum[circle_points:1]),c(yL_circumA,yL_circumB[circle_points:1]),
              col = DOTS.color)
    }
  }
  
  if(sum(RIGHT$centers == 99999)==1){
    a = 'R' #a place holder
  } else {
    for (j in 1:dim(RIGHT$centers)[1]){
      xR = RIGHT$centers[j,1]
      yR = RIGHT$centers[j,2]
      rR = RIGHT$diameters[j]/2
      xR_circum = seq(xR-rR,xR+rR, length.out = circle_points)
      temp = ifelse(rR^2 - (xR_circum-xR)^2<0,0,sqrt(rR^2 - (xR_circum-xR)^2))
      yR_circumA = temp + yR #above part
      temp = ifelse(rR^2 - (xR_circum-xR)^2<0,0,-sqrt(rR^2 - (xR_circum-xR)^2))
      yR_circumB = temp + yR #below part
      polygon(c(xR_circum,xR_circum[circle_points:1]),c(yR_circumA,yR_circumB[circle_points:1]),
              col = DOTS.color)
      #circles(x,y,r)
    }
  }
  dev.off()
  
}



#Random dot size equal area and density









