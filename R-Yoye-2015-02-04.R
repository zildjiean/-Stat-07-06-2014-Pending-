function (m,n,p,alpha,m2,n2,p2,round1) 
{
	
	#set.seed(5)
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

#cat('\n','--------------- Real UCL,CL,LCL for (L) ------------------','\n')
#cat('\n','UCL(L) =',UCLl,'CL(L) =',CLl,'LCL(L) =',LCLl,'\n')


######## Calculate CAL(R) ##########
	CLr = 1-((1-((Mbar*alpha)*alpha)))
	LCLr= CLr-(3*(sqrt(((CLr)*(1-CLr))/n)))
	UCLr= CLr+(3*(sqrt(((CLr)*(1-CLr))/n)))

#cat('\n','--------------- Real UCL,CL,LCL for (R) ------------------','\n')
#cat('\n','UCL(R) =',UCLr,'CL(R) =',CLr,'LCL(R) =',LCLr,'\n')

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
#cat('\n',"P = ",p,'\n')

	for(i in 1:round1){
		x2=rbinom(m2,n2,p2)
		z1=length(x2)
		pnew=rep(0,m2)
			j=0
			k=0
			jj=0
			kk=0


	for (v in 1:z1){
		pnew[v] = (x2[v]/n2)
		L = pnew*alpha
		R = 1-((1-pnew)*alpha)

	}
cat ('\n',"Pnew = ",pnew,'\n')
cat ('\n',"L = ",L,'\n')
cat ('\n',"R = ",R,'\n')

		in.control = TRUE

			while(in.control){

				j=j+1
				k=k+1
				

			
				if((L[k]>UCLl)||(L[k]<LCLl))
					{
							in.control=FALSE
					} 
					
					
				}
				rll[i] = j

		in.control = TRUE

			while(in.control){

				jj=jj+1
				kk=kk+1
				

			
				if((R[kk]>UCLr)||(R[kk]<LCLr))
					{
							in.control=FALSE
					} 
					
					
				}
				rlr[i] = jj

		#cat ('\n',"###################### Round = ",i,'\n')
					
		}

	cat ('\n',"RL-L = ",rll,'\n')
	cat ('\n',"RL-R = ",rlr,'\n')


######## Calculate ARL (L & R) ##########
	#arll=mean(rll)
	#arlr=mean(rll)
	#cat ('\n',"ARL-L = ",arll,'\n')
	#cat ('\n',"ARL-R = ",arlr,'\n')

}
