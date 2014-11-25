function (m,n,p,alpha,m2,n2,round1) 
{
	
	set.seed(5)
	rlr=rep(0,round1)
	rll=rep(0,round1)
	x=rbinom(m,n,p)
	z=length(x)

		for(i in 1:z){
			p[i]=(x[i]/n)
		}
		pbar=(sum(p)/m)
		cat('\n','M =',p,' ','\n')
		Mbar=pbar
cat ('\n','M bar =',Mbar,'\n')

####################### Calculate UCL,CL,LCL ##############################
######## Calculate CAL(L) ##########
	CLl = (Mbar*alpha)
	LCLl= CLl-(3*(sqrt(((CLl)*(1-CLl))/n)))
	UCLl= CLl+(3*(sqrt(((CLl)*(1-CLl))/n)))

cat('\n','--------------- Real UCL,CL,LCL for (L) ------------------','\n')
cat('\n','UCL(L) =',UCLl,'CL(L) =',CLl,'LCL(L) =',LCLl,'\n')


######## Calculate CAL(R) ##########
	CLr = 1-((1-((Mbar*alpha)*alpha)))
	LCLr= CLr-(3*(sqrt(((CLr)*(1-CLr))/n)))
	UCLr= CLr+(3*(sqrt(((CLr)*(1-CLr))/n)))

cat('\n','--------------- Real UCL,CL,LCL for (R) ------------------','\n')
cat('\n','UCL(R) =',UCLr,'CL(R) =',CLr,'LCL(R) =',LCLr,'\n')

######## Check Condition for UCL,LCL (L) ##########
		if(LCLl <= 0){
			LCLl=0
		} else LCLl=LCLl
		if(UCLl > 1){
			UCLl=1
		} else UCLl=UCLl

######## Check Condition for UCL,LCL (R) ##########
		if(LCLr <= 0){
			LCLr=0
		}else LCLr=LCLr

		if(UCLr > 1){
			UCLr=1
		} else UCLr=UCLr

cat('\n','--------------- Calculate UCL,CL,LCL for (L) ------------------','\n')
cat('\n','UCL(L) =',UCLl,'CL(L) =',CLl,'LCL(L) =',LCLl,'\n')
cat('\n','--------------- Calculate UCL,CL,LCL for (R) ------------------','\n')
cat('\n','UCL(R) =',UCLr,'CL(R) =',CLr,'LCL(R) =',LCLr,'\n')
cat('\n',"P = ",p,'\n')


######## Compare for Calculate ARL ##############
for(i in 1:round1)
{
	for(i in 1:m){
			in.control = TRUE
				j=0
				k=0

			while(in.control)
			{
				j=j+1
				k=k+1

				if(p[i] > Mbar){

				if((p[k]>UCLr)||(p[k]<LCLr))
						{
							in.control=FALSE
						}
					}

			}
			rlr[i] = j

			in.control = TRUE
				j=0
				k=0

			while(in.control)
			{
				j=j+1
				k=k+1

				if(p[i] <= Mbar){

				if((p[k]>UCLl)||(p[k]<LCL))
						{
							in.control=FALSE
						}
					}

			}
			rll[i] = j
		}
}


######## Calculate ARL ##############


cat ('\n',"RL-R = ",rlr,"RL-L = ",rll,'\n')

#ARL=mean(rl)
#cat ('\n',"ARL = ",ARL,'\n')

}
