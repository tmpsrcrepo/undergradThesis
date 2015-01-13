

# Some color functions
mygrey=function(x){
	# Input: a (vector full of a) continuous variable
	# Output: Gray colors 1:1 for data. low value are white, high values are black
	grey( (x-max(x)) / min(x-max(x)) )
}

mycols=function(x){
	# Input: a (vector full of a) continuous variable
	# Output: Blueish colors 1:1 for data. low value are white, high values are dark blue
	hcl(244,l= 99* (x-max(x)) / min(x-max(x)) )
}

mycols2=function(x){
	# Input: a (vector full of a) continuous variable
	# Output: Mono-lumi(?) darkish rainbow colors 1:1 for data. low to high [red/purple, blue, green, orangish]
	hcl(359* (x-max(x)) / min(x-max(x)), l=50 )
}

clearvars=function(mydata) {
    # Sets certain unneeded columns to zero to save space but NOT change the column numbering
#	mydata[,c(2,3,4,8,12,16,20,24,28,32,36,40)] = rep(0,length(mydata$time)*12)
	return(mydata)
}

delbman=function(mydata) {
    # Remove the "basket-man" noise based on some simple distance thresholds
	return(mydata[!markbman(mydata),])
}


markbman=function(mydata) {
    # Find the "basket-man" noise based on some simple distance thresholds
	
	distt = sqrt(mydata$SC_x^2 + mydata$SC_y^2 + mydata$SC_z^2)
#	noisy = distt>3.4898 & mydata$SC_x  < -0.99 | mydata$SC_y < -0.5 | mydata$SC_x > 1.59
#	noisy = distt>3.4898 & mydata$SC_x  < -0.979 | mydata$SC_y < -0.5 | mydata$SC_x > 1.59
	noisy = (distt>3.4898 & mydata$SC_x  < -0.901) | mydata$SC_y < -0.5 | mydata$SC_x > 1.59

	return(noisy)
}



maketruth5 = function(mydata) {
    # since the core tasks are fetch, paint, dry, load, unknown
	# We can make a "low resolution" ground truth
  #feb 15
	truth5 = mydata$TRUTH
	truth5[mydata$TRUTH == 1] = 1
	truth5[mydata$TRUTH == 2] = 2
	truth5[mydata$TRUTH == 3] = 3
	truth5[mydata$TRUTH == 4] = 4
	truth5[mydata$TRUTH == 5] = 5
	truth5[mydata$TRUTH == 6] = 6
	#truth5[mydata$TRUTH == "box"] = "load"
	#truth5[mydata$TRUTH == "load_serial"] = "load"
	#truth5[mydata$TRUTH == "toss"] = "dry"
	#truth5[mydata$TRUTH == "inspect"] = "dry"
	#truth5[mydata$TRUTH == "walk"] = "unknown"
	#truth5[mydata$TRUTH == "fetch_wip"] = "load"
	return(truth5)
}
## Some older data files used a different groudn truth labeling scheme
# maketruth5 = function(mydata) {
	# truth5 = mydata$TRUTH
	# truth5[mydata$TRUTH == "Boxing"] = "Load"
	# truth5[mydata$TRUTH == "Load_serial"] = "Load"
	# truth5[mydata$TRUTH == "Toss"] = "Dry"
	# truth5[mydata$TRUTH == "Inspect"] = "Dry"
	# truth5[mydata$TRUTH == "Walk"] = "Unknown"
	# return(truth5)
# }

runhmm=function(const=0,mydata,n=6,j=c(1:3),train_range = 1:length(mydata$TRUTH)) {
	# Fit an HMM to some data, produce graphs and accuracy tables,
	#   returns the fit model and Viterbi fits
  corner.pts=getcorners1(mydata);
  
 # colarray <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple');
  
  
	cat("Used input vars:", names(mydata[1,j]),"\n" )
	t=proc.time()[[3]]
	## using the control=list(init='KMEANS') can init the "blank" HMM using kmeans rather than random starts.
#	myhmm <- HMMFit(mydata[train_range,j],dis="NORMAL",nStates=n,control=list(init='KMEANS'))
	myhmm <- HMMFit(mydata[train_range,j],dis="NORMAL",nStates=n)
  
	cat("HMMfit elapsed time",proc.time()[[3]] - t,"\n")

	t=proc.time()[[3]]
	v <- viterbi(myhmm,mydata[,j])
 
  #Feb 15, 2014
 df = data.frame(mydata[,1],v$states,mydata$TRUTH)  
  write.csv(df, file = "novice000.csv")
  
	cat("Viterbi elapsed time",proc.time()[[3]] - t,"\n")
  colarray <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple');
  plot3d(mydata[,1:3],col=colarray[const+v$states],xlab='Right Hand X', ylab='Right Hand Y', zlab='Right Hand Z');aspect3d('iso')

#	plot3d(mydata[,9:11],col=rainbow(n)[v$states]); aspect3d("iso")
	
	# Plot the colors used, kinda like a legend
	plot(1:n,1:n,col=rainbow(n),pch=16)
	text(1:n,1:n,1:n,col=rainbow(n),pos=3)
	
	tab = table(mydata$TRUTH, v$states); # all 10 tasks
	print(tab);
	cat( "All tasks score:", sum(apply(tab,2,max)) / sum(tab) *100 ,"\n")
	tab5=table(mydata$truth5, v$states) # core 6 tasks
	cat( "5 tasks score:", sum(apply(tab5,2,max)) / sum(tab5) *100 ,"\n")
	return(list(H=myhmm,V=v,TAB=tab,TAB5=tab5))
}

getcorners=function(mydata) {
	# Find min/max for each dimension
	min_X = min(mydata$SC_x)
	max_X = max(mydata$SC_x)
	min_Y = min(mydata$SC_y)
	max_Y = max(mydata$SC_y)
	min_Z = min(mydata$SC_z)
	max_Z = max(mydata$SC_z)

	# Use min/max points to form the corners of the graphing box.
	c= matrix(c(
	min_X, min_Y, min_Z,
	min_X, min_Y, max_Z,
	min_X, max_Y, min_Z,
	min_X, max_Y, max_Z,
	max_X, min_Y, min_Z,
	max_X, min_Y, max_Z,
	max_X, max_Y, min_Z,
	max_X, max_Y, max_Z),
	byrow=TRUE, nrow=8, dimnames=list(NULL,c('x','y','z')));

	return(c);
}

Pause <- function () { 
    cat("Hit <enter> to continue...")
    readline()
    invisible()
}

etilt=function(mydata) {
	# Estimate the Y-Z plan tile of the data (in radians)
	t=atan( lm(mydata$SC_y~mydata$SC_z)$coefficients[2])
	#return(c(t, t*180/pi))
	return(t[[1]])
}

plot_tilt=function(mydata) {
	# Show the Y-Z plane with the estimated tilt line
	plot(mydata$SC_z,mydata$SC_y,col=mydata$TRUTH,cex=0.5,pch=16)
	c = lm(mydata$SC_y~mydata$SC_z)$coefficients
	abline(c[[1]],c[[2]])
}

untilt=function(mydata,x_ang=0) {
	plot_tilt(mydata)
	Pause()
	if( x_ang == 0) {
		rotx_ang = atan( lm(mydata$SC_y~mydata$SC_z)$coefficients[2] )
		cat("Estimated rotation:",rotx_ang *180/pi," degrees\n")
	} else {
		rotx_ang = x_ang *pi/180
	}
	rotx = rotationMatrix(-rotx_ang,1,0,0)[1:3,1:3]

	# In the dataset we only need to rotate the XYZ coords of joints, other fields are ignored
	# We assume the joints start at 5:7 (head) and go through 41:43 (right hand) for 10 total joints
	jindex=5:7
	for (i in 1:10 ) {
		mydata[,jindex] = as.matrix(mydata[,jindex]) %*% rotx
		jindex = jindex + 4
	}
	plot_tilt(mydata)
	return(mydata)
}

animate=function(mydata,jindex=9:11,starta=1,stopb=dim(mydata)[1],p.Per=5,sleep=0 ) {
	# This function will show some 3d plotted data in "animation" by plotting a few points at a time
	# The loop will "accelerate" after 1,000 iterations

	# Plot the corners (in white) to initialize the graphing space
	corner.pts=getcorners(mydata);
	plot3d(corner.pts,col='white');
	# Redraw with the correct aspect ratio
	aspect3d("iso")
	# Animate. At each step plot the joint in their own colors
	a = starta;
	b = a + p.Per;
	i=0; # Loop iteration counter
	while ( b < stopb ) {
		# Add in all points from "a" to "b"
		points3d(mydata[a:b,jindex],col=mydata$TRUTH[a:b])
		i = i+1;
		if(i > 1000) p.Per = p.Per + 1 #check for acceleration
		a = b + 1; # Increment "a" up to the next chunk start
		b = a + p.Per; # Increment "b" up to the next chunks end
		if( sleep>0 ) Sys.sleep(sleep) # Additional sleep to slow down frame rate
	}
}

angle_calc=function(mydata,a,b,c) {
	# Generates interior angle netween three points, i.e. angle a_b_c
	# where a, b, c are array indices for a{x,y,z}, b{x,y,z}, c{x,y,z}
	# e.g. for left elbow a=13:15;b=17:19; c=21:23;
	
	# Returns the angle in radians (for the whole length of the mydata frame)
	acos( apply( (mydata[,a]-mydata[,b]) * (mydata[,b] - mydata[,c]) ,1,sum) /
	(sqrt(apply( (mydata[,a]-mydata[,b]) * (mydata[,a]-mydata[,b]) ,1,sum)) *
	sqrt(apply( ((mydata[,b] - mydata[,c])) * (mydata[,b] - mydata[,c]) ,1,sum)) ) )
	# Warning, Some NaNs may be produced!
}

newvars=function(mydata) {
	# Generate all the supplemental "derived" variables from joints

	# dist = distance (of neck) to origin
	mydata$dist = sqrt(mydata$SC_x^2 + mydata$SC_y^2 + mydata$SC_z^2)

	# elaps = row-to-row Elapsed time
	t1 = mydata$cam_seconds
	t2 = mydata$cam_seconds
	t=length(t1)+1
	t2[2:t]=t1
	t1[t] = t2[t]
	elaps = t1-t2; # Elapsed time is now just the row difference.
	elaps[1] = 0.032; #can't leave a zero here
	mydata$elaps = elaps[1:(t-1)]; # Put it back in the original without the extra row
	rm(t1,t2,t,elaps)

	# trav = row-to-row trav distance of the neck point
	t=length(mydata$SC_x)+1
	scx = mydata$SC_x
	scy = mydata$SC_y
	scz = mydata$SC_z
	scx[2:t] = mydata$SC_x
	scy[2:t] = mydata$SC_y
	scz[2:t] = mydata$SC_z
	trav = sqrt(
	(scx[1:(t-1)]-mydata$SC_x)^2 +
	(scy[1:(t-1)]-mydata$SC_y)^2 +
	(scz[1:(t-1)]-mydata$SC_z)^2
	)
	mydata$trav = trav[1:(t-1)]
	rm(scx,scy,scz,t,trav)

	# speed = distance / time
	mydata$speed = mydata$trav / mydata$elaps
	## May want to remove bogus values
	# mydata$trav[mydata$speed >1.9] = 0
	# mydata$speed[mydata$speed >1.9] = 0

	# wrist_dist = instant difference in wrist positions
	mydata$wrist_dist = sqrt(
	(mydata$WR_x - mydata$WL_x)^2 +
	(mydata$WR_y - mydata$WL_y)^2 +
	(mydata$WR_z - mydata$WL_z)^2   )

	# elbow_dist = instant difference in elbow position
	mydata$elbow_dist = sqrt(
	(mydata$ER_x - mydata$EL_x)^2 +
	(mydata$ER_y - mydata$EL_y)^2 +
	(mydata$ER_z - mydata$EL_z)^2   )

	# LH_size = instant distance from wrist to hand joints
	mydata$LH_size = sqrt(
	(mydata$WL_x - mydata$HL_x)^2 +
	(mydata$WL_y - mydata$HL_y)^2 +
	(mydata$WL_z - mydata$HL_z)^2   )
	# right hand
	mydata$RH_size = sqrt(
	(mydata$WR_x - mydata$HR_x)^2 +
	(mydata$WR_y - mydata$HR_y)^2 +
	(mydata$WR_z - mydata$HR_z)^2   )

	# Left, wrist to shoulder distance (arm extension)
	mydata$LWS_dist = sqrt(
	(mydata$SL_x - mydata$WL_x)^2 +
	(mydata$SL_y - mydata$WL_y)^2 +
	(mydata$SL_z - mydata$WL_z)^2   )
	# right
	mydata$RWS_dist = sqrt(
	(mydata$SR_x - mydata$WR_x)^2 +
	(mydata$SR_y - mydata$WR_y)^2 +
	(mydata$SR_z - mydata$WR_z)^2   )

	# Shoulders width
	mydata$width = sqrt(
	(mydata$SL_x - mydata$SR_x)^2 +
	(mydata$SL_y - mydata$SR_y)^2 +
	(mydata$SL_z - mydata$SR_z)^2   )

	# Neck length
	mydata$neck_len = sqrt(
	(mydata$HD_x - mydata$SC_x)^2 +
	(mydata$HD_y - mydata$SC_y)^2 +
	(mydata$HD_z - mydata$SC_z)^2   )

	## slightly more generic angle calculation block
	# a,b,c are the indecies of three points B is the point where we want interior angle
	# acos( (a-b)*(b-c) / sqrt((a-b)*(a-b)) * sqrt((b-c)*(b-c)) )

	# Facing angle "cheats" by using the origin and that needs to be hard coded in this section

	# Facing_angle = instant angle from shoulder line to origin
	a=13:15 # shoulder left
	b=29:31 # Shoulder right, x,y,z
	# c = 0 #origin hard coded
	mydata$facing_angle = acos(
	apply( (mydata[,a]-mydata[,b]) * (mydata[,b] - c(0,0,0)) ,1,sum) /
	(sqrt(apply( (mydata[,a]-mydata[,b]) * (mydata[,a]-mydata[,b]) ,1,sum)) *
	sqrt(apply( ((mydata[,b] - c(0,0,0))) * (mydata[,b] - c(0,0,0)) ,1,sum)) ))
	rm(a,b)

	# Generate LEFT Elbow angle (shoulder, to elbow, to wrist)
	mydata$LE_angle=angle_calc(mydata,13:15,17:19,21:23)
	# Set NaN's to zero
	mydata$LE_angle[is.na(mydata$LE_angle)] = 0

	# Generate Right Elbow angle (shoulder, to elbow, to wrist)
	mydata$RE_angle=angle_calc(mydata,29:31,33:35,37:39)
	# Set NaN's to zero
	mydata$RE_angle[is.na(mydata$RE_angle)] = 0

	# Arm "UP" signals, e.g. wrist has positive Y value compared to shoulder
	# Wrist over Elbow
	mydata$LWoE = mydata$WL_y - mydata$EL_y
	mydata$RWoE = mydata$WR_y - mydata$ER_y
	# Elbow over Shoulder
	mydata$LEoS = mydata$EL_y - mydata$SL_y
	mydata$REoS = mydata$ER_y - mydata$SR_y
	# Wrist over Shoulder
	mydata$LWoS = mydata$WL_y - mydata$SL_y
	mydata$RWoS = mydata$WR_y - mydata$SR_y

	# Right over left (wrist)
	mydata$RWoLW = mydata$WR_y - mydata$WL_y
	# Right over left (elbow)
	mydata$REoLE = mydata$ER_y - mydata$EL_y
	
	# Generate Right Shoulder angle (neck to shoulder to elbow)
	mydata$RS_angle=angle_calc(mydata,9:11,29:31,33:35)
	# Set NaN's to zero
	mydata$RS_angle[is.na(mydata$RE_angle)] = 0

	# Generate Left Shoulder angle (neck to shoulder to elbow)
	mydata$LS_angle=angle_calc(mydata,9:11,13:15,17:19)
	# Set NaN's to zero
	mydata$LS_angle[is.na(mydata$RE_angle)] = 0
	
	return(mydata);
}

