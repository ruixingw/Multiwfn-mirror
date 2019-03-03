!!!------------------ Evaluate overlap integral for two unnormalized GTFs, a warpper of doSintactual for simplicity
real*8 function doSint(iGTF,jGTF)
integer iGTF,jGTF
real*8,external :: doSintactual
doSint=doSintactual(iGTF,jGTF,0,0,0,0,0,0)
end function
!!!------------------ Evaluate overlap integral for two unnormalized GTFs
!~p arguments are the shifts of GTF index, used by doKint, doveloint but not used by doSint
real*8 function doSintactual(iGTF,jGTF,ix1p,iy1p,iz1p,ix2p,iy2p,iz2p)
use util
use defvar
implicit real*8(a-h,o-z)
integer iGTF,jGTF,ix1p,iy1p,iz1p,ix2p,iy2p,iz2p
x1=a(b(iGTF)%center)%x
y1=a(b(iGTF)%center)%y
z1=a(b(iGTF)%center)%z
x2=a(b(jGTF)%center)%x
y2=a(b(jGTF)%center)%y
z2=a(b(jGTF)%center)%z
ee1=b(iGTF)%exp
ee2=b(jGTF)%exp
ep=ee1+ee2
sqrtep=dsqrt(ep)
px=(ee1*x1+ee2*x2)/ep
py=(ee1*y1+ee2*y2)/ep
pz=(ee1*z1+ee2*z2)/ep		
expterm=dexp( -ee1*ee2*((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)/ep )
ix1=type2ix(b(iGTF)%type)+ix1p
iy1=type2iy(b(iGTF)%type)+iy1p
iz1=type2iz(b(iGTF)%type)+iz1p
ix2=type2ix(b(jGTF)%type)+ix2p
iy2=type2iy(b(jGTF)%type)+iy2p
iz2=type2iz(b(jGTF)%type)+iz2p
!chen book,P103
numx=ceiling( (ix1+ix2+1)/2D0 ) !Need to calculate n points
sx=0.0D0
do i=1,numx
	tmp=Rhm(numx,i)/sqrtep+px
	term1=(tmp-x1)**ix1
	term2=(tmp-x2)**ix2
	sx=sx+Whm(numx,i)*term1*term2
end do
sx=sx/sqrtep

numy=ceiling( (iy1+iy2+1)/2D0 )
sy=0.0D0
do i=1,numy
	tmp=Rhm(numy,i)/sqrtep+py
	term1=(tmp-y1)**iy1
	term2=(tmp-y2)**iy2
	sy=sy+Whm(numy,i)*term1*term2
end do
sy=sy/sqrtep

numz=ceiling( (iz1+iz2+1)/2D0 )
sz=0.0D0
do i=1,numz
	tmp=Rhm(numz,i)/sqrtep+pz
	term1=(tmp-z1)**iz1
	term2=(tmp-z2)**iz2
	sz=sz+Whm(numz,i)*term1*term2
end do
sz=sz/sqrtep

doSintactual=sx*sy*sz*expterm
end function
!!!------------------ Generate overlap matrix between all GTFs
!nsize should be nprims*(nprims+1)/2
subroutine genGTFSmat(GTFSmat,nsize)
use defvar
implicit real*8 (a-h,o-z)
integer nsize
real*8 GTFSmat(nsize)
!$OMP PARALLEL DO SHARED(GTFSmat) PRIVATE(ides,iGTF,jGTF) schedule(dynamic) NUM_THREADS(nthreads)
do iGTF=1,nprims
	do jGTF=iGTF,nprims
		ides=jGTF*(jGTF-1)/2+iGTF
		GTFSmat(ides)=doSint(iGTF,jGTF)
	end do
end do
!$OMP END PARALLEL DO
end subroutine
!!!------------------ Generate overlap matrix between all basis functions
!Sbas should be allocated first. The resultant matrix is for Cartesian basis functions, may be converted to spherical-harmonic later
subroutine genSbas
use defvar
implicit real*8 (a-h,o-z)
Sbas=0D0
!$OMP PARALLEL DO SHARED(Sbas) PRIVATE(i,ii,j,jj) schedule(dynamic) NUM_THREADS(nthreads)
do i=1,nbasis
	do j=i,nbasis
		do ii=primstart(i),primend(i)
			do jj=primstart(j),primend(j)
				Sbas(i,j)=Sbas(i,j)+primconnorm(ii)*primconnorm(jj)*doSint(ii,jj)
			end do
		end do
		Sbas(j,i)=Sbas(i,j)
	end do
end do
!$OMP END PARALLEL DO
end subroutine





!!!-------- Evaluate dipole moment integral for two unnormalized GTFs. The negative charge of electron has been considered!
!~p arguments are the shifts of GTF index as doSintactual
!xint/yint/zint correspond to dipole moment integral in X/Y/Z
subroutine dodipoleint(iGTF,jGTF,ix1p,iy1p,iz1p,ix2p,iy2p,iz2p,xint,yint,zint)
use util
use defvar
implicit real*8(a-h,o-z)
real*8 xint,yint,zint
integer iGTF,jGTF,ix1p,iy1p,iz1p,ix2p,iy2p,iz2p
x1=a(b(iGTF)%center)%x
y1=a(b(iGTF)%center)%y
z1=a(b(iGTF)%center)%z
x2=a(b(jGTF)%center)%x
y2=a(b(jGTF)%center)%y
z2=a(b(jGTF)%center)%z
ee1=b(iGTF)%exp
ee2=b(jGTF)%exp
ep=ee1+ee2
sqrtep=dsqrt(ep)
px=(ee1*x1+ee2*x2)/ep
py=(ee1*y1+ee2*y2)/ep
pz=(ee1*z1+ee2*z2)/ep		
expterm=dexp( -ee1*ee2*((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)/ep )
ix1=type2ix(b(iGTF)%type)+ix1p
iy1=type2iy(b(iGTF)%type)+iy1p
iz1=type2iz(b(iGTF)%type)+iz1p
ix2=type2ix(b(jGTF)%type)+ix2p
iy2=type2iy(b(jGTF)%type)+iy2p
iz2=type2iz(b(jGTF)%type)+iz2p
!First, calculate sx,sy,sz as usual as doSint
numx=ceiling( (ix1+ix2+1)/2D0 ) !Need to calculate n points
sx=0.0D0
do i=1,numx
	tmp=Rhm(numx,i)/sqrtep+px
	term1=(tmp-x1)**ix1
	term2=(tmp-x2)**ix2
	sx=sx+Whm(numx,i)*term1*term2
end do
sx=sx/sqrtep
numy=ceiling( (iy1+iy2+1)/2D0 )
sy=0.0D0
do i=1,numy
	tmp=Rhm(numy,i)/sqrtep+py
	term1=(tmp-y1)**iy1
	term2=(tmp-y2)**iy2
	sy=sy+Whm(numy,i)*term1*term2
end do
sy=sy/sqrtep
numz=ceiling( (iz1+iz2+1)/2D0 )
sz=0.0D0
do i=1,numz
	tmp=Rhm(numz,i)/sqrtep+pz
	term1=(tmp-z1)**iz1
	term2=(tmp-z2)**iz2
	sz=sz+Whm(numz,i)*term1*term2
end do
sz=sz/sqrtep
!Second, calculate overlap integral in X,Y,Z directions but with X,Y,Z coordinate variables (relative to the original point of the whole system) to produce sxx,syy,szz
numx=ceiling( (ix1+ix2+2)/2D0 ) !Because X variable is introduced, ix1+ix2+2 is used instead of ix1+ix2+1
sxx=0.0D0
do i=1,numx
	tmp=Rhm(numx,i)/sqrtep+px
	term1=(tmp-x1)**ix1
	term2=(tmp-x2)**ix2
	sxx=sxx+Whm(numx,i)*term1*term2*tmp
end do
sxx=sxx/sqrtep
numy=ceiling( (iy1+iy2+2)/2D0 )
syy=0.0D0
do i=1,numy
	tmp=Rhm(numy,i)/sqrtep+py
	term1=(tmp-y1)**iy1
	term2=(tmp-y2)**iy2
	syy=syy+Whm(numy,i)*term1*term2*tmp
end do
syy=syy/sqrtep
numz=ceiling( (iz1+iz2+2)/2D0 )
szz=0.0D0
do i=1,numz
	tmp=Rhm(numz,i)/sqrtep+pz
	term1=(tmp-z1)**iz1
	term2=(tmp-z2)**iz2
	szz=szz+Whm(numz,i)*term1*term2*tmp
end do
szz=szz/sqrtep

xint=-sxx*sy*sz*expterm
yint=-sx*syy*sz*expterm
zint=-sx*sy*szz*expterm
end subroutine
!------ A warpper of dodipoleint, used to directly get a single component of dipole moment integral. icomp=1/2/3 corresponds to X/Y/Z component
real*8 function dipintcomp(icomp,iGTF,jGTF,ix1p,iy1p,iz1p,ix2p,iy2p,iz2p)
integer icomp,iGTF,jGTF,ix1p,iy1p,iz1p,ix2p,iy2p,iz2p
real*8 xcomp,ycomp,zcomp
call dodipoleint(iGTF,jGTF,ix1p,iy1p,iz1p,ix2p,iy2p,iz2p,xcomp,ycomp,zcomp)
if (icomp==1) dipintcomp=xcomp
if (icomp==2) dipintcomp=ycomp
if (icomp==3) dipintcomp=zcomp
end function
!!!--------------- Generate dipole moment integral matrix between all GTFs
!nsize should be nprims*(nprims+1)/2
subroutine genGTFDmat(GTFdipmat,nsize)
use defvar
implicit real*8 (a-h,o-z)
integer nsize
real*8 GTFdipmat(3,nsize)
!$OMP PARALLEL DO SHARED(GTFdipmat) PRIVATE(ides,iGTF,jGTF,xdiptmp,ydiptmp,zdiptmp) schedule(dynamic) NUM_THREADS(nthreads)
do iGTF=1,nprims
	do jGTF=iGTF,nprims
		ides=jGTF*(jGTF-1)/2+iGTF
		call dodipoleint(iGTF,jGTF,0,0,0,0,0,0,xdiptmp,ydiptmp,zdiptmp)
		GTFdipmat(1,ides)=xdiptmp
		GTFdipmat(2,ides)=ydiptmp
		GTFdipmat(3,ides)=zdiptmp
	end do
end do
!$OMP END PARALLEL DO
end subroutine
!!!------------------ Generate dipole moment integral matrix between all Cartesian basis functions
!Dbas should be allocated first. The resultant matrix is for Cartesian basis functions, may be converted to spherical-harmonic later
!An alternative way to generate Dbas is set igenDbas to 1 and reload the input file, this way can generate the matrix for both Cartesian and spherical basis functions
subroutine genDbas
use defvar
implicit real*8 (a-h,o-z)
Dbas=0D0
!$OMP PARALLEL DO SHARED(Dbas) PRIVATE(i,ii,j,jj,xdiptmp,ydiptmp,zdiptmp) schedule(dynamic) NUM_THREADS(nthreads)
do i=1,nbasis
	do j=i,nbasis
		do ii=primstart(i),primend(i)
			do jj=primstart(j),primend(j)
				call dodipoleint(ii,jj,0,0,0,0,0,0,xdiptmp,ydiptmp,zdiptmp)
				Dbas(1,i,j)=Dbas(1,i,j)+primconnorm(ii)*primconnorm(jj)*xdiptmp
				Dbas(2,i,j)=Dbas(2,i,j)+primconnorm(ii)*primconnorm(jj)*ydiptmp
				Dbas(3,i,j)=Dbas(3,i,j)+primconnorm(ii)*primconnorm(jj)*zdiptmp
			end do
		end do
		Dbas(:,j,i)=Dbas(:,i,j)
	end do
end do
!$OMP END PARALLEL DO
end subroutine
!!!-------------- Generate dipole moment integral matrix between all orbitals via unitary transformation of Dbas
!Dbas must be already filled. DorbA and DorbB are global arrays
subroutine genDorb
use defvar
integer i
do i=1,3
	DorbA(i,:,:)=matmul(matmul(transpose(CObasa),Dbas(i,:,:)),CObasa)
end do
if (allocated(CObasb)) then
	do i=1,3
		DorbB(i,:,:)=matmul(matmul(transpose(CObasb),Dbas(i,:,:)),CObasb)
	end do
end if
end subroutine





!!!------- Evaluate magnetic integral for two unnormalized GTFs 
!The imaginary sign i is ignored. Note that the negative sign of magnetic operator is not occurred here
!Consult doVelint for the method for evaluation of <a|d/dx|b>, and TCA,6,341 for the formula of magnetic integral
subroutine doMagint(iGTF,jGTF,xcomp,ycomp,zcomp)
use defvar
implicit real*8(a-h,o-z)
integer iGTF,jGTF
real*8 xcomp,ycomp,zcomp,term(4)
ee1=b(iGTF)%exp
ee2=b(jGTF)%exp
ix1=type2ix(b(iGTF)%type)
iy1=type2iy(b(iGTF)%type)
iz1=type2iz(b(iGTF)%type)
ix2=type2ix(b(jGTF)%type)
iy2=type2iy(b(jGTF)%type)
iz2=type2iz(b(jGTF)%type)
term=0
!X component, <a|y*d/dz-z*d/dy|b>. Since <a|d/dz|b>=iz2*<a|b-1z>-2*ee2*<a|b+1z>, the term such as <a|y*d/dz|b> can be evaluated in terms of dipole integrals iz2*<a|y|b-1z>-2*ee2*<a|y|b+1z>
if(iz2>0) term(1)=   iz2*dipintcomp(2,iGTF,jGTF,0,0,0,0,0,-1) !viz. iz2*<a|y|b-1z>
          term(2)=-2*ee2*dipintcomp(2,iGTF,jGTF,0,0,0,0,0, 1)
if(iy2>0) term(3)=  -iy2*dipintcomp(3,iGTF,jGTF,0,0,0,0,-1,0)
          term(4)= 2*ee2*dipintcomp(3,iGTF,jGTF,0,0,0,0, 1,0)
xcomp=-sum(term) !Note that the result of dipintcomp has a negative sign due to the negative charge of electron, so here revise the sign
term=0
!Y component, <a|z*d/dx-x*d/dz|b>
if(ix2>0) term(1)=   ix2*dipintcomp(3,iGTF,jGTF,0,0,0,-1,0,0)
          term(2)=-2*ee2*dipintcomp(3,iGTF,jGTF,0,0,0, 1,0,0)
if(iz2>0) term(3)=  -iz2*dipintcomp(1,iGTF,jGTF,0,0,0,0,0,-1)
          term(4)= 2*ee2*dipintcomp(1,iGTF,jGTF,0,0,0,0,0, 1)
ycomp=-sum(term)
term=0
!Z component, <a|x*d/dy-y*d/dx|b>
if(iy2>0) term(1)=   iy2*dipintcomp(1,iGTF,jGTF,0,0,0,0,-1,0)
          term(2)=-2*ee2*dipintcomp(1,iGTF,jGTF,0,0,0,0, 1,0)
if(ix2>0) term(3)=  -ix2*dipintcomp(2,iGTF,jGTF,0,0,0,-1,0,0)
          term(4)= 2*ee2*dipintcomp(2,iGTF,jGTF,0,0,0, 1,0,0)
zcomp=-sum(term)
end subroutine
!!!--------------- Generate magnetic dipole moment integral matrix between all GTFs
!nsize should be nprims*(nprims+1)/2
!Beware that when using this result, (j,i) element should be set to negative value of (i,j) due to the Hermitean character of this operator!
subroutine genGTFMmat(GTFdipmat,nsize)
use defvar
implicit real*8 (a-h,o-z)
integer nsize
real*8 GTFdipmat(3,nsize)
GTFdipmat=0D0
!$OMP PARALLEL DO SHARED(GTFdipmat) PRIVATE(ides,iGTF,jGTF,xdiptmp,ydiptmp,zdiptmp) schedule(dynamic) NUM_THREADS(nthreads)
do iGTF=1,nprims
	do jGTF=iGTF+1,nprims !For iGTF=jGTF, the value must exactly zero, so don't calculate
		ides=jGTF*(jGTF-1)/2+iGTF
		call doMagint(iGTF,jGTF,xdiptmp,ydiptmp,zdiptmp)
		GTFdipmat(1,ides)=xdiptmp
		GTFdipmat(2,ides)=ydiptmp
		GTFdipmat(3,ides)=zdiptmp
	end do
end do
!$OMP END PARALLEL DO
end subroutine
!!!------------------ Generate magnetic integral matrix between all basis functions
!Magbas should be allocated first. The resultant matrix is for Cartesian basis functions, may be converted to spherical-harmonic later
!Notice that the diagonal element of magnetic integral matrix is zero, and (i,j)=-(j,i) due to Hermitean character
subroutine genMagbas
use defvar
implicit real*8 (a-h,o-z)
Magbas=0D0
!$OMP PARALLEL DO SHARED(Magbas) PRIVATE(i,ii,j,jj,xcomp,ycomp,zcomp) schedule(dynamic) NUM_THREADS(nthreads)
do i=1,nbasis
	do j=i+1,nbasis
		do ii=primstart(i),primend(i)
			do jj=primstart(j),primend(j)
				call doMagint(ii,jj,xcomp,ycomp,zcomp)
				Magbas(1,i,j)=Magbas(1,i,j)+primconnorm(ii)*primconnorm(jj)*xcomp
				Magbas(2,i,j)=Magbas(2,i,j)+primconnorm(ii)*primconnorm(jj)*ycomp
				Magbas(3,i,j)=Magbas(3,i,j)+primconnorm(ii)*primconnorm(jj)*zcomp
			end do
		end do
		Magbas(:,j,i)=-Magbas(:,i,j)
	end do
end do
!$OMP END PARALLEL DO
end subroutine





!!!------- Evaluate velocity integral for two unnormalized GTFs 
!There are three components. e.g. X direction: i<a|d/dx|b>. The imaginary sign i is ignored. Note that the negative sign of momentum operator is not occurred here
!One can consult p97 of Chen's book for the derivative of GTF. Namely <a|d/dx|b>=ix2*<a|b-1x>-2*ee2*<a|b+1x>
subroutine doVelint(iGTF,jGTF,xcomp,ycomp,zcomp)
use defvar
implicit real*8(a-h,o-z)
integer iGTF,jGTF
real*8 xcomp,ycomp,zcomp
ee1=b(iGTF)%exp
ee2=b(jGTF)%exp
ix1=type2ix(b(iGTF)%type)
iy1=type2iy(b(iGTF)%type)
iz1=type2iz(b(iGTF)%type)
ix2=type2ix(b(jGTF)%type)
iy2=type2iy(b(jGTF)%type)
iz2=type2iz(b(jGTF)%type)
term1=0
if(ix2>0) term1=   ix2*doSintactual(iGTF,jGTF,0,0,0,-1,0,0)
          term2=-2*ee2*doSintactual(iGTF,jGTF,0,0,0, 1,0,0)
xcomp=term1+term2
term1=0
if(iy2>0) term1=   iy2*doSintactual(iGTF,jGTF,0,0,0,0,-1,0)
          term2=-2*ee2*doSintactual(iGTF,jGTF,0,0,0,0, 1,0)
ycomp=term1+term2
term1=0
if(iz2>0) term1=   iz2*doSintactual(iGTF,jGTF,0,0,0,0,0,-1)
          term2=-2*ee2*doSintactual(iGTF,jGTF,0,0,0,0,0, 1)
zcomp=term1+term2
end subroutine
!!!--------------- Generate velocity integral matrix between all GTFs
!nsize should be nprims*(nprims+1)/2
!Beware that when using this result, (j,i) element should be set to negative value of (i,j) due to the Hermitean character of this operator!
subroutine genGTFVelmat(GTFVelmat,nsize)
use defvar
implicit real*8 (a-h,o-z)
integer nsize
real*8 GTFVelmat(3,nsize)
!$OMP PARALLEL DO SHARED(GTFVelmat) PRIVATE(ides,iGTF,jGTF,xtmp,ytmp,ztmp) schedule(dynamic) NUM_THREADS(nthreads)
do iGTF=1,nprims
	do jGTF=iGTF,nprims
		ides=jGTF*(jGTF-1)/2+iGTF
		call doVelint(iGTF,jGTF,xtmp,ytmp,ztmp)
		GTFVelmat(1,ides)=xtmp
		GTFVelmat(2,ides)=ytmp
		GTFVelmat(3,ides)=ztmp
	end do
end do
!$OMP END PARALLEL DO
end subroutine
!!!------------------ Generate velocity integral matrix between all basis functions
!Velbas should be allocated first. The resultant matrix is for Cartesian basis functions, may be converted to spherical-harmonic later
!Notice that the diagonal element of velocity integral matrix is zero, and (i,j)=-(j,i) due to Hermitean character
subroutine genVelbas
use defvar
implicit real*8 (a-h,o-z)
Velbas=0D0
!$OMP PARALLEL DO SHARED(Velbas) PRIVATE(i,ii,j,jj,xcomp,ycomp,zcomp) schedule(dynamic) NUM_THREADS(nthreads)
do i=1,nbasis
	do j=i+1,nbasis
		do ii=primstart(i),primend(i)
			do jj=primstart(j),primend(j)
				call doVelint(ii,jj,xcomp,ycomp,zcomp)
				Velbas(1,i,j)=Velbas(1,i,j)+primconnorm(ii)*primconnorm(jj)*xcomp
				Velbas(2,i,j)=Velbas(2,i,j)+primconnorm(ii)*primconnorm(jj)*ycomp
				Velbas(3,i,j)=Velbas(3,i,j)+primconnorm(ii)*primconnorm(jj)*zcomp
			end do
		end do
		Velbas(:,j,i)=-Velbas(:,i,j)
	end do
end do
!$OMP END PARALLEL DO
end subroutine





!!!------------------- Evaluate kinetic integral (i.e. -(1/2)der2 )for two unnormalized GTFs, see Chen's book, p104
real*8 function doTint(iGTF,jGTF)
use defvar
implicit real*8(a-h,o-z)
integer iGTF,jGTF
real*8 term(4)
ee1=b(iGTF)%exp
ee2=b(jGTF)%exp
ix1=type2ix(b(iGTF)%type)
iy1=type2iy(b(iGTF)%type)
iz1=type2iz(b(iGTF)%type)
ix2=type2ix(b(jGTF)%type)
iy2=type2iy(b(jGTF)%type)
iz2=type2iz(b(jGTF)%type)
term=0
if(ix1>0.and.ix2>0)  term(1)=   ix1*ix2*doSintactual(iGTF,jGTF,-1,0,0,-1,0,0)
if(ix1>0)            term(2)=-2*ee2*ix1*doSintactual(iGTF,jGTF,-1,0,0, 1,0,0)
if(ix2>0)            term(3)=-2*ee1*ix2*doSintactual(iGTF,jGTF, 1,0,0,-1,0,0)
                     term(4)= 4*ee1*ee2*doSintactual(iGTF,jGTF, 1,0,0, 1,0,0)
Tx=sum(term)
term=0
if(iy1>0.and.iy2>0)  term(1)=   iy1*iy2*doSintactual(iGTF,jGTF,0,-1,0,0,-1,0)
if(iy1>0)            term(2)=-2*ee2*iy1*doSintactual(iGTF,jGTF,0,-1,0,0, 1,0)
if(iy2>0)            term(3)=-2*ee1*iy2*doSintactual(iGTF,jGTF,0, 1,0,0,-1,0)
                     term(4)= 4*ee1*ee2*doSintactual(iGTF,jGTF,0, 1,0,0, 1,0)
Ty=sum(term)
term=0
if(iz1>0.and.iz2>0)  term(1)=   iz1*iz2*doSintactual(iGTF,jGTF,0,0,-1,0,0,-1)
if(iz1>0)            term(2)=-2*ee2*iz1*doSintactual(iGTF,jGTF,0,0,-1,0,0, 1)
if(iz2>0)            term(3)=-2*ee1*iz2*doSintactual(iGTF,jGTF,0,0, 1,0,0,-1)
                     term(4)= 4*ee1*ee2*doSintactual(iGTF,jGTF,0,0, 1,0,0, 1)
Tz=sum(term)
doTint=(Tx+Ty+Tz)/2
end function
!!!------------------ Generate kinetic energy matrix between all GTFs
!nsize should be nprims*(nprims+1)/2
subroutine genGTFTmat(GTFTmat,nsize)
use defvar
implicit real*8 (a-h,o-z)
integer nsize
real*8 GTFTmat(nsize)
!$OMP PARALLEL DO SHARED(GTFTmat) PRIVATE(ides,iGTF,jGTF) schedule(dynamic) NUM_THREADS(nthreads)
do iGTF=1,nprims
	do jGTF=iGTF,nprims
		ides=jGTF*(jGTF-1)/2+iGTF
		GTFTmat(ides)=doTint(iGTF,jGTF)
	end do
end do
!$OMP END PARALLEL DO
end subroutine
!!!------------------ Generate kinetic energy matrix between all basis functions
!Tbas should be allocated first. The resultant matrix is for Cartesian basis functions, may be converted to spherical-harmonic later
subroutine genTbas
use defvar
implicit real*8 (a-h,o-z)
Tbas=0D0
!$OMP PARALLEL DO SHARED(Tbas) PRIVATE(i,ii,j,jj) schedule(dynamic) NUM_THREADS(nthreads)
do i=1,nbasis
	do j=i,nbasis
		do ii=primstart(i),primend(i)
			do jj=primstart(j),primend(j)
				Tbas(i,j)=Tbas(i,j)+primconnorm(ii)*primconnorm(jj)*doTint(ii,jj)
			end do
		end do
		Tbas(j,i)=Tbas(i,j)
	end do
end do
!$OMP END PARALLEL DO
end subroutine