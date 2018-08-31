pro ks_stats,dist1,dist2,kstest,cprob
;
; KS test statistics for distribtions with different number of observations
; Compute KS stastic according to Gibbons, 1985 Nonparameteric Methods 
; for Quantitative Analysis (Second Edition) page 250-252
;
; Update 3/2002 RBP VLH to accomodate any size arrays
;
;**********************************************************************
;                           test samples
;
;dist1=dist1(0:200)
;dist2=dist2(0:200)
;!p.multi=[0,1,3]
;seed=1375638112l
;error=.01*randomn(seed,100)
;for k=0,20 do begin
;x=-1.+.02*dindgen(100)
;dist1=exp(-x^2/.2)
n=n_elements(dist1)
;!linetype=0
;plot,x,dist1,title='Sample data (random error='+string(.01*k)+')'
;x=-1.+.02*dindgen(100)
;dist2=exp(-(x)^2/.2)+k*error
m=n_elements(dist2)
;!linetype=1
;oplot,x,dist2
;
;**********************************************************************
;
xmin=min(dist1)-abs(.1*min(dist1))
if min(dist2) lt xmin then xmin=min(dist2)-abs(.1*min(dist2))
;
xmax=max(dist1)+abs(.1*max(dist1))
if max(dist2) gt xmax then xmax=max(dist2)+abs(.1*max(dist2))
;
;xbin=.2
xbin=(xmax-xmin)/50.
;
hist1=histogram(dist1,max=xmax,min=xmin,binsize=xbin)
hist2=histogram(dist2,max=xmax,min=xmin,binsize=xbin)
hist1=hist1/total(hist1)
hist2=hist2/total(hist2)
;
; first define flag to tag the two distributions
;
dist1flg=1+0*fix(abs(dist1))
dist2flg=0*fix(abs(dist2))
;
; construct joint distribution
;
joint=[dist1,dist2]
jointflg=[dist1flg,dist2flg]
nm=n+m 
joint=reform(joint,nm)
jointflg=reform(jointflg,nm)
;
; sort joint distribution
;
index=sort(joint)
joint=joint(index)
jointflg=jointflg(index)
;
; compute empirical distribution functions for the two distributions
; (proportion of sample observations less than or equal to x)
;
sdist1=fltarr(nm)
sdist2=fltarr(nm)
if jointflg(0) eq 1 then begin
sdist1(0)=1./n
sdist2(0)=0.
endif
if jointflg(0) eq 0 then begin
sdist2(0)=1./m
sdist1(0)=0.
endif
for i=1l,nm-1L do begin
if jointflg(i) eq 1L then begin
sdist1(i)=sdist1(i-1)+1./n 
sdist2(i)=sdist2(i-1)
endif
if jointflg(i) eq 0 then begin
sdist2(i)=sdist2(i-1)+1./m
sdist1(i)=sdist1(i-1)
endif
endfor
;!linetype=0
;plot,sdist1,title='Empirical Distribution Function'
;!linetype=1
;oplot,sdist2
;
; Kolmogorov-Smirnov test statistic (D) is the maximum absolute difference
; between the two empirical distributions functions
;
kstest=max(abs(sdist2-sdist1))
;print,'d=',kstest 
;
; Construct Asymptotic Null Distribution for two-sided KS test
; valid in the limit of large n&m
; (from Pratt and Gibbons, Concepts of Nonparametric Theory, pg 329) 
;
lamba=0.
num=2*n_elements(dist1)
dnm=dindgen(num)/(1.*num)
p=fltarr(num)
for kk=0L,num-1L do begin
    lamba=dnm(kk)*sqrt(1.*n*m/(n+m))
    p(kk)=0.
    for i=1L,num do begin
        sign=(-1)^(i+1)
        term=sign*exp(-2.*(i*lamba)^2)
;       print,'term=',term,lamba
        if abs(term) le 1.e-23 then goto, jumpout
        pold=p(kk)
        p(kk)=p(kk)+term
        if p(kk) eq pold then goto, jumpout
    endfor
    jumpout:
    p(kk)=2.*p(kk)
;   print,'pkk=',p(kk)
endfor
;
; find probablity for computed KS statistic 
;
cprob=0.
for kk=1L,num-1L do begin
if kstest ge dnm(kk-1) and kstest lt dnm(kk) then begin
scale=(kstest-dnm(kk))/(dnm(kk-1)-dnm(kk))
cprob=p(kk)+scale*(p(kk-1)-p(kk)) 
;print,'KS significance=',100.*cprob,' %'
goto,jumpout1
endif
endfor
jumpout1:
;
; find KS statistic for 95% probability 
;
sdnm=0.
prob=.95
for kk=1L,num-1L do begin
if prob le p(kk-1) and prob gt p(kk) then begin
scale=(prob-p(kk))/(p(kk-1)-p(kk))
sdnm=dnm(kk)+scale*(dnm(kk-1)-dnm(kk)) 
goto,jumpout2
endif
endfor
jumpout2:
;;
;; note: significance level is the maximum probability of rejecting a true null
;; hypothesis which in this case is that the distributions are equal
;;
;; therefore: the minimum probability that the null hypothesis is true is 1-significance

;x0=xmin 
;x1=xmax 
;y0=0 
;y1=max(hist1) 
;if max(hist2) gt y1 then y1=max(hist2) 
;y1=y1+.5*(y1-y0)
;!psym=10
;!linetype=0
;x=xmin+xbin*dindgen(n_elements(hist1))
;plot,x,hist1,xrange=[xmin,xmax],yrange=[y0,y1],ytitle='Frequency',$
;title=' Freq. Dist.'+' (n='+strmid(string(n),8,4)+$
;                        ' m='+strmid(string(m),8,4)+')'
;xyouts,x0+.05*(x1-x0),y1-.2*(y1-y0),$
;'KS significance='+strmid(string(100.*cprob),5,5)+' %'
;oplot,[x1-.3*(x1-x0),x1-.2*(x1-x0)],[y1-.2*(y1-y0),y1-.2*(y1-y0)]
;xyouts,x1-.2*(x1-x0),y1-.2*(y1-y0),' sample 1'
;!linetype=1
;oplot,x,hist2
;oplot,[x1-.3*(x1-x0),x1-.2*(x1-x0)],[y1-.3*(y1-y0),y1-.3*(y1-y0)]
;!psym=0
;!linetype=0
;xyouts,x1-.2*(x1-x0),y1-.3*(y1-y0),' sample 2'
;endfor
return
end
