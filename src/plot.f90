module plot
use defvar
use dislin_d
use util
implicit real*8(a-h,o-z)

contains


!!!------------- Draw molecular structure and show isosurface of orbitals or grid data
!Also draw topology analysis results, surface extrema, basin or domain space etc.
subroutine drawmol
use topo
use surfvertex
use basinintmod
IMPLICIT real*8(a-h,o-z)
integer i,j,idxtmp,iret,screenx,screeny,ipath,ipathp1,ipt,icp1,icp2,ipathtype,ipathmidpt,isurf,interval
real*8 abslenx,absleny,abslenz,plotlenx,plotleny,plotlenz !absolute and real 3D coordinate
real*8 plot2abs,xplotcoor,yplotcoor,absx,absy,absz,dist,textheighmod,extsiz,augplotlen
real*8 trianglex(3),triangley(3),trianglez(3)
real*8 arrayx(nx),arrayy(ny),arrayz(nz)
character*5 ctemp
!Set viewpoint
!Note that due to limitation of DISLIN, it is not possible to view molecule in all viewpoints. The YVU should be limited to between -90 and 90, else the viewpoint will suddently flip
XVUold=XVU
YVUold=YVU
if (YVU==90) YVU=89.999D0 !Temporarily modify YVU, otherwise when YVU equals to 90 or -90, the perspective will jump suddenly
if (YVU==-90) YVU=-89.999D0

!Determine axis range
augplotlen=8D0 !Total augment of plotting axis length (the sum of both direction)
if (GUI_mode==5) augplotlen=12D0 !Because surface extreme points laying on vdW surface, the scatter region is large, so use larger value
if (GUI_mode==7) augplotlen=12D0 !Using GUI to set box, the case is complicated, so use larger value
if ((idrawisosur==1.and.aug3D>4).or.GUI_mode==6) augplotlen=aug3D*2.2D0 !Shouldn't be 2.0 as expected, otherwise sometimes there will be a band occuring at boundary
!For grd file or cube file (with no or 1 atom, which may be added by Multiwfn), determine displayed spatial scope purely by grid data scope
if ( (ifiletype==7.and.(ncenter==0.or.ncenter==1)) .or.ifiletype==8) then
	xlow=orgx
	ylow=orgy
	zlow=orgz
	plotlenx=(nx-1)*dx
	plotleny=(ny-1)*dy
	plotlenz=(nz-1)*dz
else
	extsiz=augplotlen/2D0 !Extension length of axis in each side
	xlow=minval(a%x)-extsiz
	ylow=minval(a%y)-extsiz
	zlow=minval(a%z)-extsiz
	plotlenx=(maxval(a%x)-minval(a%x)+augplotlen) !Length of plotted molecular space
	plotleny=(maxval(a%y)-minval(a%y)+augplotlen)
	plotlenz=(maxval(a%z)-minval(a%z)+augplotlen)
end if
xhigh=xlow+plotlenx
yhigh=ylow+plotleny
zhigh=zlow+plotlenz

!Initialize DISLIN
abslenx=2D0 !Absolute length in DISLIN
absleny=abslenx*plotleny/plotlenx
abslenz=abslenx*plotlenz/plotlenx
plot2abs=abslenx/plotlenx !The relationship between molecular coordinate and absolute coordinate
if (isavepic==0) then
	call METAFL('CONS')
	if (GUI_mode==6) call METAFL('CONS') !Namely showing basin, using opengl by default to accelerate displaying, however when savepic, if still use opengl, things cannot be properly shown
	CALL setxid(idisgraph, 'WIDGET')
else if (isavepic==1) then
	CALL setxid(0,'NONE')
	call METAFL(graphformat)
	call winsiz(graph3Dwidth,graph3Dheight) !Actual image size is set by this routine, namely 770*770
end if
CALL SCRMOD('revers')
CALL PAGE(3000,3000)
CALL IMGFMT("RGB") !If don't call this routine, the saved picture use palette color mode by default, the color is not smooth
CALL DISINI
call ERRMOD("ALL","OFF")
! call linmod("ON","SMOOTH") !It seems that Anti-aliased doesn't take effect
CALL LABDIG(1,"X")
CALL LABDIG(1,"Y")
CALL LABDIG(1,"Z")
if (ishowaxis==0) call NOGRAF
CALL VIEW3D(XVU,YVU,ZVU,"ANGLE")
CALL erase

!Set font
if (isavepic==0) then
	CALL HWFONT
else if (isavepic==1) then
	call HELVES
	CALL SHDCHA
end if
if (isavepic==0.and.isys==1) then
	call height(60)
	CALL HNAME(60)
else
	call height(50)
	CALL HNAME(50)
end if

!Set axis
CALL NAME('X-axis (Bohr)', 'x')
CALL NAME('Y-axis (Bohr)', 'y')
CALL NAME('Z-axis (Bohr)', 'z')
CALL NAMDIS(100,'xyz')
call axis3D(abslenx,absleny,abslenz)
CALL AXSPOS(400,2600)
CALL AX3LEN(2600,2600,2600)
call labl3D("horizontal")
CALL LIGHT('on')
CALL SHDMOD('SMOOTH','SURFACE') !By default, each surface triangle has the same color and thus doesn't look smooth
nsclx=8
spcx=ceiling(plotlenx/nsclx) !Expected step size in X
spcy=ceiling(plotleny/ (plotleny/plotlenx*nsclx) )
spcz=ceiling(plotlenz/ (plotlenz/plotlenx*nsclx) /1.5D0 )
shiftx=mod(xlow,spcx) !Shift the initial point of the axis to make a label just occur at 0.0
shifty=mod(ylow,spcy)
shiftz=mod(zlow,spcz)
! CALL FLAB3D !If use this, the starting label of Y and Z axis will be plotted, however this may suppress the starting label of X axis
CALL GRAF3D(xlow,xhigh,xlow-shiftx,spcx, ylow,yhigh,ylow-shifty,spcy, zlow,zhigh,zlow-shiftz,spcz)
if (ishowaxis==1) CALL GRID3D (2,2,'bottom')
CALL ZBFINI(IRET) !Enable Z-buffer to determine visibility
call litpos(1,XVU,YVU,ZVU,'ANGLE')
call litmod(1,'on') !Dislin default light 1 is on, and off for others
if (idrawmol==1) then
	do ilight=2,8
		call litmod(ilight,'off')
	end do
	!Draw atoms
	do i=1,ncenter
		CALL MATOP3(atm3Dclr(a(i)%index,1), atm3Dclr(a(i)%index,2), atm3Dclr(a(i)%index,3), 'diffuse')
		if (a(i)%index/=0) then !Normal atoms
			CALL SPHE3D(a(i)%x,a(i)%y,a(i)%z,vdwr(a(i)%index)/4*ratioatmsphere,50,50)
		else !Bq atoms. The size keeps fixed
			CALL SPHE3D(a(i)%x,a(i)%y,a(i)%z,vdwr(a(i)%index)/4,50,50)
		end if
	end do
	!Draw bonds
	CALL MATOP3(bondclrR,bondclrG,bondclrB,'diffuse')
	do i=1,ncenter
		do j=i+1,ncenter
			if (a(i)%index==0.or.a(j)%index==0) cycle !Don't link Bq
			dist=dsqrt( (a(i)%x-a(j)%x)**2+(a(i)%y-a(j)%y)**2+(a(i)%z-a(j)%z)**2 )
			!if the distance between to atoms exceed 15% of summing of their covalent radius, seems they didn't bond
			if (dist<( covr(a(i)%index)+covr(a(j)%index) )*bondcrit) CALL TUBE3D(a(i)%x,a(i)%y,a(i)%z,a(j)%x,a(j)%y,a(j)%z,bondradius,30,30)
		end do
	end do
end if

!Draw critical points
if (ishow3n3==1) then
	CALL MATOP3(CP3n3RGB(1),CP3n3RGB(2),CP3n3RGB(3), 'diffuse') !Purple
	do i=1,numcp
		if (CPtype(i)==1) CALL SPHE3D(CPpos(1,i),CPpos(2,i),CPpos(3,i),0.19D0*ratioCPsphere,20,20)
	end do
end if
if (ishow3n1==1) then
	CALL MATOP3(CP3n1RGB(1),CP3n1RGB(2),CP3n1RGB(3), 'diffuse') !Orange
	do i=1,numcp
		if (CPtype(i)==2) CALL SPHE3D(CPpos(1,i),CPpos(2,i),CPpos(3,i),0.12D0*ratioCPsphere,20,20)
	end do
end if
if (ishow3p1==1) then
	CALL MATOP3(CP3p1RGB(1),CP3p1RGB(2),CP3p1RGB(3), 'diffuse') !Yellow
	do i=1,numcp
		if (CPtype(i)==3) CALL SPHE3D(CPpos(1,i),CPpos(2,i),CPpos(3,i),0.12D0*ratioCPsphere,20,20)
	end do
end if
if (ishow3p3==1) then
	CALL MATOP3(CP3p3RGB(1),CP3p3RGB(2),CP3p3RGB(3), 'diffuse') !Green
	do i=1,numcp
		if (CPtype(i)==4) CALL SPHE3D(CPpos(1,i),CPpos(2,i),CPpos(3,i),0.12D0*ratioCPsphere,20,20)
	end do
end if

!Show trace of movement to attractor in basin generating process. If you want to enable this, move declaration of "ntrjgrid" and "trjgrid" from "generatebasin" to basinintmod
! CALL MATOP3(0.1D0, 0.5D0, 0.85D0, 'diffuse')
! do igrd=2,ntrjgrid
! 	attx=orgx+(trjgrid(igrd,1)-1)*dx
! 	atty=orgy+(trjgrid(igrd,2)-1)*dy
! 	attz=orgz+(trjgrid(igrd,3)-1)*dz
! 	CALL SPHE3D(attx,atty,attz,0.05D0,20,20)
! 	call tube3D(attx,atty,attz,orgx+(trjgrid(igrd-1,1)-1)*dx,orgy+(trjgrid(igrd-1,2)-1)*dy,orgz+(trjgrid(igrd-1,3)-1)*dx,0.015D0,30,30)
! end do

!For basin integration analysis
if (numatt>0.and.ishowatt==1) then
	!Draw attractors
	do iatt=1,numatt
		if (attval(iatt)>=0) then
			CALL MATOP3(0.65D0, 0.9D0, 0.45D0, 'diffuse')
		else
			CALL MATOP3(0.35D0, 0.7D0, 0.9D0, 'diffuse')
		end if
		CALL SPHE3D(attxyz(iatt,1),attxyz(iatt,2),attxyz(iatt,3),attsphsize,20,20)
	end do
	!Draw basin
	if (idrawbasinidx/=-10) then !-10 means don't draw basins
		if (idrawbasinidx<=0) then !Positive basin or the basins consist of boundary or unassigned grids
			CALL MATOP3(0D0, 0.8D0, 0D0, 'diffuse') !Green
		else if (realattval(idrawbasinidx)>=0) then
			CALL MATOP3(0D0, 0.8D0, 0D0, 'diffuse') !Green
		else !Negative basins
			CALL MATOP3(0.3D0, 0.6D0, 1D0, 'diffuse') !light blue
		end if
		tmpsphrad=dsqrt(dx**2+dy**2+dz**2)/2D0
		do iz=2,nz-1
			do iy=2,ny-1
				do ix=2,nx-1
					if (gridbas(ix,iy,iz)==idrawbasinidx) then
						tmpx=orgx+(ix-1)*dx
						tmpy=orgy+(iy-1)*dy
						tmpz=orgz+(iz-1)*dz
						if (interbasgrid(ix,iy,iz)==.true.) CALL SPHE3D(tmpx,tmpy,tmpz,tmpsphrad,4,4)
						if (idrawinternalbasin==1) then
							if (ix==2.or.ix==nx-1.or.iy==2.or.iy==ny-1.or.iz==2.or.iz==nz-1) CALL QUAD3D(tmpx,tmpy,tmpz,dx,dy,dz)
						end if
					end if
				end do
			end do
		end do
	end if
end if

!Draw domain defined by isosurface as grids
if (idrawdomain==1.and.idrawdomainidx/=0) then
	CALL MATOP3(0D0, 0.8D0, 0D0, 'diffuse') !Green
	tmpsphrad=dsqrt(dx**2+dy**2+dz**2)/2D0
	do igrd=1,domainsize(idrawdomainidx)
		idx=domaingrid(idrawdomainidx,igrd)
		CALL SPHE3D(gridxyz(idx,1),gridxyz(idx,2),gridxyz(idx,3),tmpsphrad,4,4)
	end do
end if

!Draw topology paths
if (idrawpath==1) then
	do ipath=1,numpath
		call path_cp(ipath,icp1,icp2,ipathtype)
		!ipathtype=0: other   =1: (3,-1)->(3,-3) =2: (3,+1)->(3,+3) =3: (3,-1)<-->(3,+1)
		if (ipathtype==0) call MATOP3(0.8D0, 0.8D0, 0.8D0, 'diffuse')
		if (ipathtype==1) call MATOP3(0.9D0, 0.7D0, 0.0D0, 'diffuse')
		if (ipathtype==2) call MATOP3(0.2D0, 0.7D0, 0.2D0, 'diffuse')
		if (ipathtype==3) call MATOP3(0.8D0, 0.8D0, 0.3D0, 'diffuse')
		do ipt=2,pathnumpt(ipath)
			CALL TUBE3D(topopath(1,ipt-1,ipath),topopath(2,ipt-1,ipath),topopath(3,ipt-1,ipath)&
			,topopath(1,ipt,ipath),topopath(2,ipt,ipath),topopath(3,ipt,ipath),0.03D0,5,5)
		end do
	end do
end if

!Draw interbasin surfaces
if (idrawbassurf==1) then
	if (isurfstyle==1) then !A bunch of paths to represent the interbasin surface
		call MATOP3(0.7D0, 0.7D0, 0.8D0, 'diffuse')
		do isurf=1,numbassurf
			do ipath=1,nsurfpathpercp
				do ipt=2,nsurfpt
					CALL TUBE3D(bassurpath(1,ipt-1,ipath,isurf),bassurpath(2,ipt-1,ipath,isurf),bassurpath(3,ipt-1,ipath,isurf)&
					,bassurpath(1,ipt,ipath,isurf),bassurpath(2,ipt,ipath,isurf),bassurpath(3,ipt,ipath,isurf),0.005D0,5,5)
				end do
			end do
		end do
	else if (isurfstyle==2) then !Use triangles to consist the interbasin surfaces
		call MATOP3(1D0,1D0,1D0,'ambient')
		interval=2
		do isurf=1,numbassurf
			do ipath=1,nsurfpathpercp
				ipathp1=ipath+1
				if (ipath==nsurfpathpercp) ipathp1=1
				do ipt=1,nsurfpt-interval,interval
					!Use two triangle to comprise a tile of face. Define the two triangle in clockwise manner first
					trianglex(:)=(/ bassurpath(1,ipt,ipath,isurf),bassurpath(1,ipt+interval,ipath,isurf),bassurpath(1,ipt+interval,ipathp1,isurf) /)
					triangley(:)=(/ bassurpath(2,ipt,ipath,isurf),bassurpath(2,ipt+interval,ipath,isurf),bassurpath(2,ipt+interval,ipathp1,isurf) /)
					trianglez(:)=(/ bassurpath(3,ipt,ipath,isurf),bassurpath(3,ipt+interval,ipath,isurf),bassurpath(3,ipt+interval,ipathp1,isurf) /)
					call TRIA3D(trianglex,triangley,trianglez)
					trianglex(:)=(/ bassurpath(1,ipt,ipath,isurf),bassurpath(1,ipt+interval,ipathp1,isurf),bassurpath(1,ipt,ipathp1,isurf) /)
					triangley(:)=(/ bassurpath(2,ipt,ipath,isurf),bassurpath(2,ipt+interval,ipathp1,isurf),bassurpath(2,ipt,ipathp1,isurf) /)
					trianglez(:)=(/ bassurpath(3,ipt,ipath,isurf),bassurpath(3,ipt+interval,ipathp1,isurf),bassurpath(3,ipt,ipathp1,isurf) /)
					call TRIA3D(trianglex,triangley,trianglez)
					!Define the two triangle in anti-clockwise manner. We must draw the surfaces two times from different view,
					!otherwise the surface will be invisible from certain viewpoint
					trianglex(:)=(/ bassurpath(1,ipt,ipath,isurf),bassurpath(1,ipt+interval,ipathp1,isurf),bassurpath(1,ipt+interval,ipath,isurf) /)
					triangley(:)=(/ bassurpath(2,ipt,ipath,isurf),bassurpath(2,ipt+interval,ipathp1,isurf),bassurpath(2,ipt+interval,ipath,isurf) /)
					trianglez(:)=(/ bassurpath(3,ipt,ipath,isurf),bassurpath(3,ipt+interval,ipathp1,isurf),bassurpath(3,ipt+interval,ipath,isurf) /)
					call TRIA3D(trianglex,triangley,trianglez)
					trianglex(:)=(/ bassurpath(1,ipt,ipath,isurf),bassurpath(1,ipt,ipathp1,isurf),bassurpath(1,ipt+interval,ipathp1,isurf) /)
					triangley(:)=(/ bassurpath(2,ipt,ipath,isurf),bassurpath(2,ipt,ipathp1,isurf),bassurpath(2,ipt+interval,ipathp1,isurf) /)
					trianglez(:)=(/ bassurpath(3,ipt,ipath,isurf),bassurpath(3,ipt,ipathp1,isurf),bassurpath(3,ipt+interval,ipathp1,isurf) /)
					call TRIA3D(trianglex,triangley,trianglez)
				end do
			end do
		end do
	else if (isurfstyle==3) then !Use a lot of cylinders between adjacent paths to portray interbasin surfaces
		call MATOP3(1D0,1D0,1D0,'diffuse')
		do isurf=1,numbassurf
			do ipath=1,nsurfpathpercp
				ipathp1=ipath+1
				if (ipath==nsurfpathpercp) ipathp1=1
				do ipt=1,nsurfpt
					CALL TUBE3D(bassurpath(1,ipt,ipath,isurf),bassurpath(2,ipt,ipath,isurf),bassurpath(3,ipt,ipath,isurf)&
					,bassurpath(1,ipt,ipathp1,isurf),bassurpath(2,ipt,ipathp1,isurf),bassurpath(3,ipt,ipathp1,isurf),0.03D0,3,3)
				end do
			end do
		end do
	end if
end if

!Draw spheres corresponding to local minimum and maximum derived from molecular surface analysis
if (GUI_mode==5) then
	if (ishowlocminpos==1) then
		CALL MATOP3(0D0, 0D0, 1D0, 'diffuse') !Blue
		do i=1,nsurlocmin
			idxtmp=surlocminidx(i)
			if (idxtmp==0) cycle !Has been discarded by user
			CALL SPHE3D(survtx(idxtmp)%x,survtx(idxtmp)%y,survtx(idxtmp)%z,0.15D0,20,20)
		end do
	end if
	if (ishowlocmaxpos==1) then
		CALL MATOP3(1D0, 0.0D0, 0.0D0, 'diffuse') !Red
		do i=1,nsurlocmax
			idxtmp=surlocmaxidx(i)
			if (idxtmp==0) cycle
			CALL SPHE3D(survtx(idxtmp)%x,survtx(idxtmp)%y,survtx(idxtmp)%z,0.15D0,20,20)
		end do
	end if
end if

!When isosur1style==5(transparent), isosur2style must be also 5. For transparent style, TPRINI/TPRFIN conflict with Z-buffer, so here we need to recall ZBFFIN earlier
!When one of isosur1style and isosur2style is unequal to 5, then another must not be 5. Overall, we ensure that the circumstance that only one isosurface is transparent will not occured
if (isosur1style==5) CALL ZBFFIN

if (idrawisosur==1) then
	!Set lighting parameter for showing both isosurface 1 and 2
 	call litpos(1,XVU,YVU,ZVU,'ANGLE')
	call litpos(2,XVU+90,YVU+90,ZVU,'ANGLE')
	call litpos(3,XVU+90,YVU-90,ZVU,'ANGLE')
 	call litpos(4,XVU-90,YVU+90,ZVU,'ANGLE')
 	call litpos(5,XVU-90,YVU-90,ZVU,'ANGLE')
!  	call litpos(6,XVU+50,YVU,ZVU,'ANGLE')
!  	call litpos(7,XVU,YVU+50,ZVU,'ANGLE')
!  	call litpos(8,XVU-50,YVU,ZVU,'ANGLE')
!  	call litpos(8,-2*abslenx,2*absleny,2.5*abslenz,'ANGLE')
	if (ienablelight1==1) call litmod(1,'on')
	if (ienablelight2==1) call litmod(2,'on')
	if (ienablelight3==1) call litmod(3,'on')
	if (ienablelight4==1) call litmod(4,'on')
	if (ienablelight5==1) call litmod(5,'on')
	if (ienablelight1==0) call litmod(1,'off')
	if (ienablelight2==0) call litmod(2,'off')
	if (ienablelight3==0) call litmod(3,'off')
	if (ienablelight4==0) call litmod(4,'off')
	if (ienablelight5==0) call litmod(5,'off')

	!Draw isosurface for cubmat
	do ix=1,nx
		arrayx(ix)=orgx+(ix-1)*dx
	end do
	do iy=1,ny
		arrayy(iy)=orgy+(iy-1)*dy
	end do
	do iz=1,nz
		arrayz(iz)=orgz+(iz-1)*dz
	end do
	if (isosur1style==5) CALL TPRVAL(opacitycub1)
	nplottime=1
	if (isosurshowboth==1) nplottime=2 !Show both positive and negative region
	do iplottime=1,nplottime
		CALL MATOP3(clrRcub1same,clrGcub1same,clrBcub1same,'diffuse') !Set color for solid isosurface 1 with the same sign of set isovalue
		call setrgb(clrRcub1samemeshpt,clrGcub1samemeshpt,clrBcub1samemeshpt)
		sur_valuenow=sur_value
		if (iplottime==2) then
			CALL MATOP3(clrRcub1oppo,clrGcub1oppo,clrBcub1oppo,'diffuse') !Set color for solid isosurface 1 with the opposite sign of set isovalue
			call setrgb(clrRcub1oppomeshpt,clrGcub1oppomeshpt,clrBcub1oppomeshpt)
			sur_valuenow=-sur_value
		end if
		if (isosur1style==2) call surmsh("LINES") !Plotted as mesh rather than solid face
		if (isosur1style==3) call surmsh("POINTS") !Plotted as points rather than solid face
		if (isosur1style==4) call surmsh("ON") !face+lines
		if (isosur1style==5) CALL TPRINI !Must be called individually for same and opposite survalue, else same part will completely overlay opposite part
		call suriso(arrayx,nx,arrayy,ny,arrayz,nz,cubmat,sur_valuenow)
		if (isosur1style==5) CALL TPRFIN
		call surmsh("OFF") !If don't set this, then other things (atoms, bonds) will be drawn as LINES too
	end do
	
	!Draw isosurface for cubmattmp at the same time
	if (isosur2style==5) CALL TPRVAL(opacitycub2)
	if (isosursec==1.and.allocated(cubmattmp)) then
		nplottime=1
		if (isosurshowboth==1) nplottime=2 !Show both positive and negative region
		do iplottime=1,nplottime
			CALL MATOP3(clrRcub2same,clrGcub2same,clrBcub2same,'diffuse') !Set color for solid isosurface 2 with the same sign of set isovalue
			call setrgb(clrRcub2samemeshpt,clrGcub2samemeshpt,clrBcub2samemeshpt)
			sur_valuenow=sur_value
			if (iplottime==2) then
				CALL MATOP3(clrRcub2oppo,clrGcub2oppo,clrBcub2oppo,'diffuse') !Set color for solid isosurface 2 with the opposite sign of set isovalue
				call setrgb(clrRcub2oppomeshpt,clrGcub2oppomeshpt,clrBcub2oppomeshpt)
				sur_valuenow=-sur_value
			end if
			if (isosur2style==2) call surmsh("LINES") !Plotted as mesh rather than solid face
			if (isosur2style==3) call surmsh("POINTS") !Plotted as points rather than solid face
			if (isosur2style==4) call surmsh("ON") !face+lines
			if (isosur1style==5) CALL TPRINI
			call suriso(arrayx,nx,arrayy,ny,arrayz,nz,cubmattmp,sur_valuenow)
			if (isosur1style==5) CALL TPRFIN
			call surmsh("OFF")
		end do
	end if
end if

!Draw a 3D rectangle box to show spatial range of present grid data or grid data to be calculated
if (ishowdatarange==1) then 
	CALL MATOP3(0.0D0, 0.0D0, 0.8D0, 'diffuse')
	call tube3D(orgx,orgy,orgz,endx,orgy,orgz,0.05D0,30,30)
	call tube3D(endx,orgy,orgz,endx,endy,orgz,0.05D0,30,30)
	call tube3D(endx,endy,orgz,orgx,endy,orgz,0.05D0,30,30)
	call tube3D(orgx,endy,orgz,orgx,orgy,orgz,0.05D0,30,30)
	call tube3D(orgx,orgy,endz,endx,orgy,endz,0.05D0,30,30)
	call tube3D(endx,orgy,endz,endx,endy,endz,0.05D0,30,30)
	call tube3D(endx,endy,endz,orgx,endy,endz,0.05D0,30,30)
	call tube3D(orgx,endy,endz,orgx,orgy,endz,0.05D0,30,30)
	call tube3D(orgx,orgy,orgz,orgx,orgy,endz,0.05D0,30,30)
	call tube3D(endx,orgy,orgz,endx,orgy,endz,0.05D0,30,30)
	call tube3D(endx,endy,orgz,endx,endy,endz,0.05D0,30,30)
	call tube3D(orgx,endy,orgz,orgx,endy,endz,0.05D0,30,30)
end if

if (isosur1style/=5) CALL ZBFFIN !Ending of Z-buffer

!Write atom name, index of atoms/CPs/paths/surface extremes/real attractors to 3D plot
if (ishowatmlab==1.or.ishowCPlab==1.or.ishowpathlab==1.or.ishowlocminlab==1.or.ishowlocmaxlab==1.or.(ishowattlab==1.and.numatt>0)) then
	textheighmod=textheigh+155*plot2abs**1.3D0 !Change text size according to molecule size
	call height(int(textheighmod))
	if (ishowatmlab==1) then
		call setRGB(atmlabclrR,atmlabclrG,atmlabclrB)
		do i=1,ncenter
			write(ctemp,"(i5)") i
			absx=(a(i)%x-(maxval(a%x)+minval(a%x))/2) * plot2abs !Find atomic absolute coordinate
			absy=(a(i)%y-(maxval(a%y)+minval(a%y))/2) * plot2abs
 			absz=(a(i)%z-(maxval(a%z)+minval(a%z))/2) * plot2abs
			call abs3pt(absx,absy,absz,xplotcoor,yplotcoor) !Convert atomic absolute coordinate to screen coordinate(pixel)
			screeny=nint(yplotcoor-textheighmod/1.8D0)
			if (iatmlabtype3D==1) then !Show element
				screenx=nint(xplotcoor-textheighmod/2D0)
				if (a(i)%name(2:2)/=" ") screenx=nint(xplotcoor-textheighmod/1.3D0)
				call messag(trim(a(i)%name),screenx,screeny)
			else if (iatmlabtype3D==2.or.iatmlabtype3D==4) then !Show index all atoms / Bq atoms
				if (iatmlabtype3D==4.and.a(i)%index/=0) cycle
				screenx=nint(xplotcoor-textheighmod/2D0)
				if (i>=10) screenx=nint(xplotcoor-textheighmod/1.7D0)
				call messag(ADJUSTL(ctemp),screenx,screeny)
			else !Show element and index
				if (i<10) then !Slightly move center of text so that they can at center of atom
					screenx=nint(xplotcoor-textheighmod/1.1D0)
				else !Move in X more, since the index has two or more digitals
					screenx=nint(xplotcoor-textheighmod/0.8D0)
				end if
				call messag(trim(a(i)%name)//ADJUSTL(ctemp),screenx,screeny)
			end if
		end do
	end if
	if (ishowCPlab==1) then
		CALL SETRGB(CPlabclrR,CPlabclrG,CPlabclrB)
		do i=1,numcp
			if (CPtype(i)==1.and.ishow3n3==0) cycle
			if (CPtype(i)==2.and.ishow3n1==0) cycle
			if (CPtype(i)==3.and.ishow3p1==0) cycle
			if (CPtype(i)==4.and.ishow3p3==0) cycle
			write(ctemp,"(i5)") i
			absx=(CPpos(1,i)-(maxval(a%x)+minval(a%x))/2) * plot2abs !Find atomic absolute coordinate
			absy=(CPpos(2,i)-(maxval(a%y)+minval(a%y))/2) * plot2abs
 			absz=(CPpos(3,i)-(maxval(a%z)+minval(a%z))/2) * plot2abs
			call abs3pt(absx,absy,absz,xplotcoor,yplotcoor) !Convert atomic absolute coordinate to screen coordinate(pixel)
			screenx=nint(xplotcoor-textheighmod/2.6)
			screeny=nint(yplotcoor-textheighmod/1.8)
			call messag(trim(ADJUSTL(ctemp)),screenx,screeny)
		end do
	end if
	if (ishowpathlab==1) then
		call color("RED")
		do ipath=1,numpath
			write(ctemp,"(i5)") ipath
			ipathmidpt=nint(pathnumpt(ipath)/2D0)
			absx=(topopath(1,ipathmidpt,ipath)-(maxval(a%x)+minval(a%x))/2) * plot2abs !Find atomic absolute coordinate
			absy=(topopath(2,ipathmidpt,ipath)-(maxval(a%y)+minval(a%y))/2) * plot2abs
 			absz=(topopath(3,ipathmidpt,ipath)-(maxval(a%z)+minval(a%z))/2) * plot2abs
			call abs3pt(absx,absy,absz,xplotcoor,yplotcoor) !Convert atomic absolute coordinate to screen coordinate(pixel)
			screenx=nint(xplotcoor-textheighmod/2.6D0)
			screeny=nint(yplotcoor-textheighmod/1.8D0)
			call messag(trim(ADJUSTL(ctemp)),screenx,screeny)
		end do
	end if
	if (ishowlocminlab==1) then
		call color("MAGENTA")
		do i=1,nsurlocmin
			idxtmp=surlocminidx(i)
			if (idxtmp==0) cycle
			write(ctemp,"(i5)") i
			absx=(survtx(idxtmp)%x-(maxval(a%x)+minval(a%x))/2) * plot2abs !Find absolute coordinate
			absy=(survtx(idxtmp)%y-(maxval(a%y)+minval(a%y))/2) * plot2abs
 			absz=(survtx(idxtmp)%z-(maxval(a%z)+minval(a%z))/2) * plot2abs
 			call abs3pt(absx,absy,absz,xplotcoor,yplotcoor) !Convert atomic absolute coordinate to screen coordinate(pixel)
			screenx=nint(xplotcoor-textheighmod/2.6D0)
			screeny=nint(yplotcoor-textheighmod/1.8D0)
			call messag(trim(ADJUSTL(ctemp)),screenx,screeny)
		end do
	end if
	if (ishowlocmaxlab==1) then
		call color("GREEN")
		do i=1,nsurlocmax
			idxtmp=surlocmaxidx(i)
			if (idxtmp==0) cycle
			write(ctemp,"(i5)") i
			absx=(survtx(idxtmp)%x-(maxval(a%x)+minval(a%x))/2) * plot2abs !Find absolute coordinate
			absy=(survtx(idxtmp)%y-(maxval(a%y)+minval(a%y))/2) * plot2abs
 			absz=(survtx(idxtmp)%z-(maxval(a%z)+minval(a%z))/2) * plot2abs
 			call abs3pt(absx,absy,absz,xplotcoor,yplotcoor) !Convert atomic absolute coordinate to screen coordinate(pixel)
			screenx=nint(xplotcoor-textheighmod/2.6D0)
			screeny=nint(yplotcoor-textheighmod/1.8D0)
			call messag(trim(ADJUSTL(ctemp)),screenx,screeny)
		end do
	end if
	if (ishowattlab==1.and.numatt>0) then
		CALL SETRGB(0.70D0,0.3D0,0.9D0)
		do iatt=1,numatt
			irealatt=attconv(iatt)
			write(ctemp,"(i5)") irealatt
			absx=(attxyz(iatt,1)-(maxval(a%x)+minval(a%x))/2) * plot2abs !Find atomic absolute coordinate
			absy=(attxyz(iatt,2)-(maxval(a%y)+minval(a%y))/2) * plot2abs
 			absz=(attxyz(iatt,3)-(maxval(a%z)+minval(a%z))/2) * plot2abs
			call abs3pt(absx,absy,absz,xplotcoor,yplotcoor) !Convert atomic absolute coordinate to screen coordinate(pixel)
			screenx=nint(xplotcoor-textheighmod/2.6)
			screeny=nint(yplotcoor-textheighmod/1.8)
			call messag(trim(ADJUSTL(ctemp)),screenx,screeny)
		end do
	end if
end if
CALL DISFIN
XVU=XVUold
YVU=YVUold
end subroutine



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!------------------------- Draw property in a line
!if atomr1==atomr2, then don't plot two points to highlight nuclear positions
!Input coordinate must be in Bohr
subroutine drawcurve(curvex,curvey,N,curvexmin,curvexmax,steplabx,curveymin,curveymax,steplaby,status,atomr1,atomr2)
implicit real*8 (a-h,o-z)
real*8 curvex(N),curvey(N),atompointx(2),atompointy(2),curvexmin,curvexmax,curveymin,curveymax,steplabx,steplaby
real*8,optional :: atomr1,atomr2
real*8 vertlinex(2),vertliney(2)
integer N
character*4 status
!Set conversion factor, determined by global variable ilenunit1D
scll=1D0 !Default, namely Bohr as unit of X-axis
if (ilenunit1D==2) scll=b2a !Conver X-axis unit to Angstrom

call SCRMOD('REVERSE')
CALL PAGE(2970,2100)
CALL IMGFMT("RGB")
CALL setxid(0,'NONE') !If we don't set this, after we draw a graph embedded in GUI(e.g. relif map), curve map will not be shown 
if (status=="show") then
	call METAFL('xwin')
	call window(200,100,900,600)
else if (status=="save") then
	call METAFL(graphformat)
	call winsiz(graph1Dwidth,graph1Dheight)
end if
CALL DISINI
! call LINMOD ('ON','SMOOTH') !It seems that Anti-aliased doesn't take effect
if (status=="show".and.isys==1) then
	call WINTIT("Curve graph, click right mouse button to continue...")
	call height(45)
	CALL HNAME(45)
else
	call height(40) !The text shown in graphic file is strangely larger than window, so slight decrease it
end if
CALL HWFONT
call center
nysize=nint(2300*curvexyratio)
call AXSLEN(2300,nysize)
if (nysize>1800) call AXSLEN(nint(2300/(nysize/1800D0)),1800)
if (ilog10y==0) call AXSSCL('lin','Y')
if (ilog10y==1) call AXSSCL('log','Y')
if (ilenunit1D==1) CALL NAME('Position (Bohr)','X')
if (ilenunit1D==2) CALL NAME('Position (Angstrom)','X')
CALL NAME('Value (a.u.)','Y')
CALL LABDIG(numdiglinex,"X")
CALL LABDIG(numdigliney,"Y")
CALL TICPOS("REVERS","XYZ")
call ERRMOD("ALL","OFF")
shifty=mod(curveymin,steplaby)
if (ilog10y==0) then
	CALL GRAF(curvexmin*scll,curvexmax*scll,curvexmin*scll,steplabx, curveymin,curveymax,curveymin-shifty,steplaby)
else if (ilog10y==1) then
	CALL GRAF(curvexmin*scll,curvexmax*scll,curvexmin*scll,steplabx, curveymin,curveymax,curveymin-shifty,1D0)
end if
CALL SETRGB(0.6D0,0.6D0,0.6D0)
CALL DASH
CALL XAXGIT !Draw the dashed line of Y=0

if (icurve_vertlinex==1) then
	vertlinex=curve_vertlinex !Draw a vertical line throughing out the whole graph to help locating special position
	if (ilog10y==0) then
		vertliney(1)=curveymin
		vertliney(2)=curveymax
	else if (ilog10y==1) then
		vertliney(1)=10**(curveymin)
		vertliney(2)=10**(curveymax)
	end if
	CALL CURVE(vertlinex,vertliney,2)
end if

call setcolor(iclrcurve)
CALL SOLID
CALL LINWID(icurvethick)
CALL CURVE(curvex*scll,curvey,N)
CALL LINWID(1)

if (present(atomr1)) then !Draw position of the two atom selected
	atompointx(1)=atomr1*scll !the position of atom shown in curve graph
	atompointx(2)=atomr2*scll
	atompointy=curveymin
	CALL SETRGB(0.9D0,0.0D0,0.0D0)
	CALL INCMRK(-1)
	CALL MARKER(21)
	CALL HSYMBL(20)
	CALL CURVE(atompointx,atompointy,2)
end if
CALL DISFIN
end subroutine



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!------------ Draw scatter graph for two functions
!This routine can also simultaneously plot two set of scatter data, as long as arrayx2/y2 are defined, they will be plotted as red points
!iratio=1: graph is 4:3  =2: graph is 1:1
subroutine drawscatter(arrayx,arrayy,ntot,xmin,xmax,ymin,ymax,iratio,stringx,stringy,arrayx2,arrayy2,ntot2)
real*8 arrayx(:),arrayy(:),xmin,xmax,ymin,ymax
integer ntot,iratio
character(len=*),optional :: stringx,stringy
integer,optional :: ntot2
real*8,optional :: arrayx2(:),arrayy2(:)
call SCRMOD('REVERSE')
if (iratio==1) then
	CALL PAGE(3000,2250) !4:3
	if (isavepic==0) then
		call METAFL('xwin')
		call window(100,100,800,600)
	else if (isavepic==1) then
		call METAFL(graphformat)
		call winsiz(graph1Dwidth,nint(graph1Dwidth/4D0*3D0)) !Ensure 4:3
	end if
else if (iratio==2) then
	CALL PAGE(3000,3000) !4:3
	if (isavepic==0) then
		call METAFL('xwin')
		call window(100,100,700,700)
	else if (isavepic==1) then
		call METAFL(graphformat)
		call winsiz(graph1Dwidth,graph1Dwidth) !Ensure 1:1
	end if
end if
CALL setxid(0,'NONE')
CALL DISINI
if (isavepic==0) call WINTIT("Scatter graph between two functions, click right mouse button to continue")
call ERRMOD("ALL","OFF")
call hwfont
call center
if (iratio==1) then
	call AXSLEN(2400,1800) !4:3
else if (iratio==2) then
	call AXSLEN(2400,2400) !1:1
end if
CALL HNAME(45)
CALL height(40)
call TEXMOD("ON")
if (present(stringx)) then
	CALL NAME(stringx,'X')
else
	CALL NAME('Function 1 value','X')
end if
if (present(stringy)) then
	CALL NAME(stringy,'Y')
else
	CALL NAME('Function 2 value','Y')
end if
CALL LABDIG(numdiglinex,"X")
CALL LABDIG(numdigliney,"Y")
CALL TICPOS("REVERS","XYZ")
CALL GRAF(xmin,xmax,xmin,(xmax-xmin)/10,ymin,ymax,ymin,(ymax-ymin)/10)
CALL INCMRK(-1)
CALL MARKER(21)
CALL HSYMBL(symbolsize)
call curve(arrayx,arrayy,ntot)
if (present(arrayx2)) then
	call color("RED")
	call curve(arrayx2,arrayy2,ntot2)
	call color("WHITE") !Restore to default (black)
end if
CALL DISFIN
end subroutine



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!----------Draw a matrix as color-mapped graph
!mat is the 2D matrix to be plotted, the dimension is numx,numy
!ninterpo determines the number of interpolation
!x/ymin,x/ymax are the lower and upper limit of X and Y axes
!zmin and zmax are the lower and upper limit of color bar
!stepx and stepy are stepsizes between labels of X and Y axes
!If nlabdig=-1, the labels of X and Y axes will be integer, this is suitable for natively representation of a matrix. If =n, there will be n digitals
!textheight is size of label, optional, default is 45
!nlabdigz is the number of digial of labels in Z axis, optional, default is 4
subroutine drawmatcolor(mat,numx,numy,xmin,xmax,ymin,ymax,zmin,zmax,stepx,stepy,ninterpo,nlabdig,textheight,nlabdigz)
implicit real*8 (a-h,o-z)
integer numx,numy,ninterpo,nlabdig
real*8 mat(numx,numy),xmin,xmax,ymin,ymax,zmin,zmax,stepx,stepy
integer :: lengthx=2300
integer,optional :: textheight,nlabdigz
call SCRMOD('REVERSE')
CALL setxid(0,'NONE')
CALL PAGE(3200,2700)
if (isavepic==0) then
	call METAFL('xwin')
	call window(200,100,900,770)
else if (isavepic==1) then
	call METAFL(graphformat)
	call winsiz(graph2Dwidth,graph2Dheight) !Because color bar need to draw, so width is bigger than height
	CALL IMGFMT('RGB')
end if
call DISINI
call ERRMOD("ALL","OFF") !If don't set this, when atom label in contour map is out of page range, DISLIN annoys users
call HWFONT
CALL LABDIG(nlabdig,"X") !-1 means integer label
CALL LABDIG(nlabdig,"Y")
if (present(nlabdigz)) then
	CALL LABDIG(nlabdigz,"Z")
else
	CALL LABDIG(4,"Z")
end if
if (present(textheight)) then
	call height(textheight)
else
	call height(45)
end if
call ticks(1,"XYZ")
call WINTIT("Colored matrix")
call center
call AUTRES(numx,numy)
call AX3LEN(lengthx,nint(lengthx*dfloat(numy)/numx),nint(lengthx*dfloat(numy)/numx))
if (ninterpo==1) then !Don't interpolate
	call sursze(1D0,dfloat(numx),1D0,dfloat(numx)) !Manually set center position of starting and ending grids to ensure boundary grids have the same size as internal grids
	call GRAF3(xmin-0.5D0,xmax+0.5D0,xmin,stepx,ymin-0.5D0,ymax+0.5D0,ymin,stepy,zmin,zmax,zmin,(zmax-zmin)/10D0)
else
	call GRAF3(xmin,xmax,xmin,stepx,ymin,ymax,ymin,stepy,zmin,zmax,zmin,(zmax-zmin)/10D0)
end if
call CRVMAT(mat,numx,numy,ninterpo,ninterpo)
call DISFIN
end subroutine



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!------------------------- Draw property on a plane (or output critical points and paths, when iplaneoutall=1)
!init and end is coordinate range in each dimension
!Input length unit must be in Bohr, but you can use ilenunit2D to change the displayed length unit (=1/2 denote Bohr/Angstrom)
subroutine drawplane(init1inp,end1inp,init2inp,end2inp,init3,end3,idrawtype)
use topo
implicit real*8 (a-h,o-z)
real*8 init1inp,end1inp,init2inp,end2inp,init1,end1,init2,end2,init3,end3
real*8 xcoord(ngridnum1),ycoord(ngridnum2),gradd1tmp(ngridnum1,ngridnum2),gradd2tmp(ngridnum1,ngridnum2)
real*8 dx,dy,pix2usr,n1,n2
real*8 planetrunc(ngridnum1,ngridnum2) !Store truncated planemat
integer lengthx !length of x axis
integer idrawtype,i,j
integer :: inplane(ncenter+1) !If =1, then the label is close enough to the plotting plane 
character*20 atmlabtext,tmpstr
scll=1D0
if (ilenunit2D==2) scll=b2a
init1=init1inp*scll !We use init1inp as an intermediate variable rather than directly use init1, because orgx2D will be passed as init1, we don't want to alternate orgx2D
init2=init2inp*scll
end1=end1inp*scll
end2=end2inp*scll
disshowlabel=disshowlabel*scll
if (iplaneoutall==1) goto 10 !Don't plot anything, but only output coordinate of critical points and paths, and then directly return
if (ilenunit2D==2) call convgridlenunit(1) !Convert plane parameters to Angstrom. At the end of the routine, they will be converted back

lengthx=2300
call SCRMOD('REVERSE')
if (isavepic==0) then
	if (idrawtype==3.or.idrawtype==4.or.idrawtype==5) then
		CALL setxid(idisgraph, 'WIDGET')
		call METAFL('CONS')
		CALL PAGE(3000,3000)
	else
		CALL setxid(0,'NONE')
		call METAFL('xwin')
		call window(200,100,900,770)
		CALL PAGE(3200,2700)
	end if
else if (isavepic==1) then
	CALL setxid(0,'NONE')
	call METAFL(graphformat)
	if (idrawtype==3.or.idrawtype==4.or.idrawtype==5) then
		call winsiz(graph3Dwidth,graph3Dheight)
	else
		call winsiz(graph2Dwidth,graph2Dheight) !Because color bar need to draw, so width is bigger than height
	end if
	if (idrawtype==3.or.idrawtype==4.or.idrawtype==5) then
		CALL PAGE(3000,3000)
	else
		CALL PAGE(3200,2700)
	end if
	CALL IMGFMT('RGB')
end if
call DISINI
call ERRMOD("ALL","OFF") !If don't set this, when atom label in contour map is out of page range, DISLIN annoys users
CALL VIEW3D(XVU,YVU,ZVU,"ANGLE")
CALL erase
CALL HWFONT
if (itickreverse==1) CALL TICPOS("REVERSE","XYZ")
CALL LABDIG(numdigx,"X")
CALL LABDIG(numdigy,"Y")
CALL LABDIG(numdigz,"Z")
if (isavepic==0.and.isys==1) then
	call height(60)
	CALL HNAME(50)
else
	call height(50)
	CALL HNAME(45)
end if
call ticks(iticks,"XYZ")
if (ilenunit2D==1) CALL NAME('Length unit: Bohr', 'x')
if (ilenunit2D==2) CALL NAME('Length unit: Angstrom', 'x')
dx=(end1-init1)/(ngridnum1-1)
dy=(end2-init2)/(ngridnum2-1)
do i=1,ngridnum1
	xcoord(i)=init1+(i-1)*dx
end do
do i=1,ngridnum2
	ycoord(i)=init2+(i-1)*dy
end do
shiftx=mod(init1,planestpx) !Shift of axis origin, so that there is a label just at exactly midpoint
shifty=mod(init2,planestpy)
shiftz=0
! shiftz=mod(init3,planestpz)

!1:Color-filled map, 2:Contour line map, 6:Gradient lines map, 7:Vector field map
if (idrawtype==1.or.idrawtype==2.or.idrawtype==6.or.idrawtype==7) then
	if (isavepic==0) then
		if (idrawtype==1) call WINTIT("Color-filled map, click right mouse button to continue")
		if (idrawtype==2) call WINTIT("Contour line map, click right mouse button to continue")
		if (idrawtype==6) call WINTIT("Gradient line map, click right mouse button to continue")
		if (idrawtype==7) call WINTIT("Contour lines with gradient lines, click right mouse button to continue")
		if (idrawtype==8) call WINTIT("Contour lines + vector field, click right mouse button to continue")
	end if
	call center
	!Length of Y is determined according to X by proportion, if length of Y axis is too large, then scale length x (and thus length y, automatically)
	if ( lengthx*(end2-init2)/(end1-init1)>2400 ) lengthx=2400/(end2-init2)*(end1-init1)
	lengthy=int(lengthx*(end2-init2)/(end1-init1))

	if (idrawtype==1) then
		call AUTRES(ngridnum1,ngridnum2)
		call AX3LEN(lengthx,lengthy,lengthy) !The length of color bar is identical to Y axis
		call GRAF3(init1,end1,init1-shiftx,planestpx, init2,end2,init2-shifty,planestpy, init3,end3,init3-shiftz,planestpz)
		if (inowhiteblack==1) then !Truncate the values larger than and lower than color scale, so that these regions will not be shown as white and black, respectively
			planetrunc=planemat
			where (planetrunc>end3) planetrunc=end3-1D-10   !Augment by a minimal value to avoid numerical noise
			where (planetrunc<init3) planetrunc=init3+1D-10
			call CRVMAT(planetrunc,ngridnum1,ngridnum2,fillcoloritpx,fillcoloritpy)
		else
			call CRVMAT(planemat,ngridnum1,ngridnum2,fillcoloritpx,fillcoloritpy)
		end if
	else
		call axslen(lengthx,lengthy)
		call GRAF(init1,end1,init1-shiftx,planestpx, init2,end2,init2-shifty,planestpy)
	end if

! 	CALL LINWID(10) !Draw an arrow 
! 	call RLvec(0D0,0.216790D0,0D0,0.216790D0-0.229901D0*3,1421)
! 	call RLvec(1.424912D0,-0.867160D0,1.424912D0+0.17136000D0*3,-0.867160D0-0.14455800D0*3,1421)
! 	CALL LINWID(1)

	if (idrawcontour==1) then !Draw contour, may be also for gradient lines and vector field map
		if (ilabel_on_contour==1) then  !Enable showing isovalue on contour lines
			call height(ictrlabsize)
			call labels('float','contur')
			call labdig(numdigctr,'contur')
		end if
		do i=1,ncontour  !Use different type of line to draw contour line
			if (ctrval(i)>=0) then
				CALL LINWID(iwidthposctr)
				call setcolor(iclrindctrpos)
				nsizestyle=2
				if (ctrposstyle(2)==0) nsizestyle=1
				call myline(ctrposstyle,nsizestyle) !Set line style
				call contur(xcoord,ngridnum1,ycoord,ngridnum2,planemat,ctrval(i))
				if (iorbsel2/=0) call contur(xcoord,ngridnum1,ycoord,ngridnum2,planemattmp,ctrval(i)) !Also plot another orbital if it was specified
			else if (ctrval(i)<0) then
				CALL LINWID(iwidthnegctr)
				call setcolor(iclrindctrneg)
				nsizestyle=2
				if (ctrnegstyle(2)==0) nsizestyle=1
				call myline(ctrnegstyle,nsizestyle) !Set line style
				call contur(xcoord,ngridnum1,ycoord,ngridnum2,planemat,ctrval(i))
				if (iorbsel2/=0) call contur(xcoord,ngridnum1,ycoord,ngridnum2,planemattmp,ctrval(i))
			end if
		end do
		if (allocated(boldlinelist)) then
			CALL LINWID(10)
			do i=1,size(boldlinelist)
				if (ctrval(boldlinelist(i))>=0.0D0) call solid
				if (ctrval(boldlinelist(i))<0.0D0) call dashm
				call contur(xcoord,ngridnum1,ycoord,ngridnum2,planemat,ctrval(boldlinelist(i)))		
			end do
			CALL LINWID(1) !Restore to default
		end if
	end if

	if (idrawtype==6) then !Draw gradient line map
		call solid
		call setcolor(iclrindgradline)
		if (igrad_arrow==1) call stmmod('on','arrow')
		if (igrad_arrow==0) call stmmod('off','arrow')
		call stmval(gradplotstep,'step')
		call stmval(gradplotdis,'distance') !controls number of line, smaller value means more lines
		call stmval(gradplottest,'test')
		call stmval(0.3D0,'arrows') !set interval distance of arrows in the same line
		call LINWID(iwidthgradline)
		call stream(gradd1,gradd2,ngridnum1,ngridnum2,xcoord,ycoord,(/ 0.0D0 /),(/ 0.0D0 /),0)
		CALL LINWID(1) !Restore to default
	else if (idrawtype==7) then !Draw vector field map
		gradd1tmp=gradd1 !Refresh gradd1,d2 array in each time
		gradd2tmp=gradd2
		do i=1,ngridnum1
			do j=1,ngridnum2
				rnorm=dsqrt(gradd1tmp(i,j)**2+gradd2tmp(i,j)**2)
				if (rnorm>cutgradvec) then
					gradd1tmp(i,j)=gradd1tmp(i,j)/rnorm*cutgradvec
					gradd2tmp(i,j)=gradd2tmp(i,j)/rnorm*cutgradvec
				end if
			end do
		end do
		if (iinvgradvec==1) gradd1tmp=-gradd1tmp
		if (iinvgradvec==1) gradd2tmp=-gradd2tmp
		if (icolorvecfield==1) then
			call vecclr(-2)
		else
			call vecclr(vecclrind)
		end if
		call vecmat(gradd1tmp,gradd2tmp,ngridnum1,ngridnum2,xcoord,ycoord,1501)
	end if
end if

!Drawing of color-filled, contour, vector maps... has been finished, below we draw more widgets on the map

pix2usr=(end1-init1)/lengthx  !Convert actual pixel to user coordinate

!Construct "inplane" list, if =1, the atom or reference point is close enough to the plotting plane
!The "inplane" will be used for plotting atomic labels and bonds later
inplane=0
nallpoints=ncenter
if (imarkrefpos==1) nallpoints=ncenter+1
do ipt=1,nallpoints
	if (ipt<=ncenter) then
		posmarkx=a(ipt)%x*scll; posmarky=a(ipt)%y*scll; posmarkz=a(ipt)%z*scll
	else if (ipt==ncenter+1) then !Plot reference point of correlation hole/factor, source function...
		posmarkx=refx*scll; posmarky=refy*scll; posmarkz=refz*scll
	end if
	if (plesel==1) then
		if (abs(posmarkz-orgz2D)<disshowlabel) inplane(ipt)=1
	else if (plesel==2) then
		if (abs(posmarky-orgy2D)<disshowlabel) inplane(ipt)=1
	else if (plesel==3) then
		if (abs(posmarkx-orgx2D)<disshowlabel) inplane(ipt)=1
	else if (plesel==4.or.plesel==5.or.plesel==6.or.plesel==7) then
		call pointprjple(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,posmarkx,posmarky,posmarkz,prjx,prjy,prjz)
		if ((prjx-posmarkx)**2+(prjy-posmarky)**2+(prjz-posmarkz)**2<disshowlabel**2) inplane(ipt)=1
	end if
end do

!Plot bonds as lines
if (ibond_on_plane==1) then
	call setcolor(iclrindbndlab); call solid; call LINWID(10)
	do ipt=1,ncenter
		posmarkx=a(ipt)%x*scll
		posmarky=a(ipt)%y*scll
		posmarkz=a(ipt)%z*scll
		if (inplane(ipt)==0) cycle
		if (plesel<=3) then !XY plane
			do iatm=ipt+1,ncenter
				if (inplane(iatm)==0) cycle
				if (distmat(ipt,iatm) < ( covr(a(ipt)%index)+covr(a(iatm)%index) )*bondcrit) then
					if (plesel==1) call rline(posmarkx,posmarky,a(iatm)%x*scll,a(iatm)%y*scll)
					if (plesel==2) call rline(posmarkx,posmarkz,a(iatm)%x*scll,a(iatm)%z*scll)
					if (plesel==3) call rline(posmarky,posmarkz,a(iatm)%y*scll,a(iatm)%z*scll)
				end if
			end do
		else if (plesel<=7) then !Atom defined by three atoms/points
			call pointprjple(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,posmarkx,posmarky,posmarkz,prjx,prjy,prjz)
			!Get position of ipt in the plotting coordinate. Comment can be found in similar part below
			if (abs(v1x*v2y-v2x*v1y)>1D-8) then
				det2_2=v1x*v2y-v2x*v1y
				n1=( (prjx-orgx2D)*v2y-v2x*(prjy-orgy2D) )/det2_2
				n2=( v1x*(prjy-orgy2D)-(prjx-orgx2D)*v1y )/det2_2
			else if (abs(v1x*v2z-v2x*v1z)>1D-8) then
				det2_2=v1x*v2z-v2x*v1z
				n1=( (prjx-orgx2D)*v2z-v2x*(prjz-orgz2D) )/det2_2
				n2=( v1x*(prjz-orgz2D)-(prjx-orgx2D)*v1z )/det2_2
			else if (abs(v1y*v2z-v2y*v1z)>1D-8) then
				det2_2=v1y*v2z-v2y*v1z
				n1=( (prjy-orgy2D)*v2z-v2y*(prjz-orgz2D) )/det2_2
				n2=( v1y*(prjz-orgz2D)-(prjy-orgy2D)*v1z )/det2_2
			end if
			if ( n1>0.and.n2>0.and.n1<(ngridnum1-1).and.n2<(ngridnum2-1) ) then !The ipt is within the scope of drawing range
				do iatm=ipt+1,ncenter
					if (inplane(iatm)==0) cycle
					if (distmat(ipt,iatm) < ( covr(a(ipt)%index)+covr(a(iatm)%index) )*bondcrit) then
						!Get position of iatm in the plotting coordinate
						call pointprjple(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,a(iatm)%x*scll,a(iatm)%y*scll,a(iatm)%z*scll,prjx,prjy,prjz)
						if (abs(v1x*v2y-v2x*v1y)>1D-8) then
							det2_2=v1x*v2y-v2x*v1y
							n1_2=( (prjx-orgx2D)*v2y-v2x*(prjy-orgy2D) )/det2_2
							n2_2=( v1x*(prjy-orgy2D)-(prjx-orgx2D)*v1y )/det2_2
						else if (abs(v1x*v2z-v2x*v1z)>1D-8) then
							det2_2=v1x*v2z-v2x*v1z
							n1_2=( (prjx-orgx2D)*v2z-v2x*(prjz-orgz2D) )/det2_2
							n2_2=( v1x*(prjz-orgz2D)-(prjx-orgx2D)*v1z )/det2_2
						else if (abs(v1y*v2z-v2y*v1z)>1D-8) then
							det2_2=v1y*v2z-v2y*v1z
							n1_2=( (prjy-orgy2D)*v2z-v2y*(prjz-orgz2D) )/det2_2
							n2_2=( v1y*(prjz-orgz2D)-(prjy-orgy2D)*v1z )/det2_2
						end if
						if ( n1_2>0.and.n2_2>0.and.n1_2<(ngridnum1-1).and.n2_2<(ngridnum2-1) ) then
							call rline(n1*d1,n2*d2,n1_2*d1,n2_2*d2)
						end if
					end if
				end do
			end if
		end if
	end do
	CALL LINWID(1) !Restore to default
end if

!Draw atomic labels or reference point on graph
if (iatom_on_plane==1) then
	call height(pleatmlabsize)
	movetext=pleatmlabsize/2 !Slight movement on the labels to make the center of label just appear at expected position
	do ipt=1,nallpoints
 		call SERIF
		CALL SHDCHA
		if (ipt<=ncenter) then !Plot atomic label on plane
			posmarkx=a(ipt)%x*scll; posmarky=a(ipt)%y*scll; posmarkz=a(ipt)%z*scll
	    	if (iatmlabtype==1) then !Only plot element for atomic label
	    	    atmlabtext=a(ipt)%name
	    	else if (iatmlabtype==2.or.iatmlabtype==3) then !Plot index
	    	    write(atmlabtext,"(i6)") ipt
	    	    atmlabtext=adjustl(atmlabtext)
				tmpstr=atmlabtext
	    	    if (iatmlabtype==3) write(atmlabtext,"(a,a)") trim(a(ipt)%name),trim(tmpstr) !Plot both element and index
	        end if
	        call setcolor(iclrindatmlab)
		else !Plot reference point of correlation hole/factor, source function...
			posmarkx=refx*scll; posmarky=refy*scll; posmarkz=refz*scll
			call color('blue')
			call HSYMBL(pleatmlabsize+20)
		end if
		if (plesel==1) then !XY plane
			if (inplane(ipt)==1) then !Close enough to the plane
				if (ipt<=ncenter) call rlmess(trim(atmlabtext),posmarkx-movetext*pix2usr,posmarky+movetext*pix2usr)
				if (ipt==ncenter+1) call rlsymb(8,posmarkx,posmarky)
			else if (iatom_on_plane_far==1) then !Far away from the plane
				call DUPLX
				call rlmess(trim(atmlabtext),posmarkx-movetext*pix2usr,posmarky+movetext*pix2usr)
			end if
		else if (plesel==2) then !XZ plane
			if (inplane(ipt)==1) then
				if (ipt<=ncenter) call rlmess(trim(atmlabtext),posmarkx-movetext*pix2usr,posmarkz+movetext*pix2usr)
				if (ipt==ncenter+1) call rlsymb(8,posmarkx,posmarkz)
			else if (iatom_on_plane_far==1) then
				call DUPLX
				call rlmess(trim(atmlabtext),posmarkx-movetext*pix2usr,posmarkz+movetext*pix2usr)
			end if
		else if (plesel==3) then !YZ plane
			if (inplane(ipt)==1) then
				if (ipt<=ncenter) call rlmess(trim(atmlabtext),posmarky-movetext*pix2usr,posmarkz+movetext*pix2usr)
				if (ipt==ncenter+1) call rlsymb(8,posmarky,posmarkz)
			else if (iatom_on_plane_far==1) then
				call DUPLX
				call rlmess(trim(atmlabtext),posmarky-movetext*pix2usr,posmarkz+movetext*pix2usr)
			end if
		else if (plesel==4.or.plesel==5.or.plesel==6.or.plesel==7) then
			call pointprjple(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,posmarkx,posmarky,posmarkz,prjx,prjy,prjz)
			!prjx-orgx2D=n1*v1x+n2*v2x, n1*d1 equals x in user coordinate. prjy as well.  prjx,y,z is projected position from posmark point
			!prjy-orgy2D=n1*v1y+n2*v2y
			!prjz-orgz2D=n1*v1z+n2*v2z
			!Use Kramer rule to solve this linear equation to get n1 and n2
			!We can use any two conditions, the precondition is det2_2 is not almost zero
			if (abs(v1x*v2y-v2x*v1y)>1D-8) then
				det2_2=v1x*v2y-v2x*v1y
				n1=( (prjx-orgx2D)*v2y-v2x*(prjy-orgy2D) )/det2_2
				n2=( v1x*(prjy-orgy2D)-(prjx-orgx2D)*v1y )/det2_2
			else if (abs(v1x*v2z-v2x*v1z)>1D-8) then
				det2_2=v1x*v2z-v2x*v1z
				n1=( (prjx-orgx2D)*v2z-v2x*(prjz-orgz2D) )/det2_2
				n2=( v1x*(prjz-orgz2D)-(prjx-orgx2D)*v1z )/det2_2
			else if (abs(v1y*v2z-v2y*v1z)>1D-8) then
				det2_2=v1y*v2z-v2y*v1z
				n1=( (prjy-orgy2D)*v2z-v2y*(prjz-orgz2D) )/det2_2
				n2=( v1y*(prjz-orgz2D)-(prjy-orgy2D)*v1z )/det2_2
			end if
			if (n1>0.and.n2>0.and.n1<(ngridnum1-1).and.n2<(ngridnum2-1)) then !Avoid atom label out of range
				if (inplane(ipt)==1) then
					if (ipt<=ncenter) call rlmess(trim(atmlabtext),n1*d1-movetext*pix2usr,n2*d2+movetext*pix2usr)
					if (ipt==ncenter+1) call rlsymb(8,n1*d1,n2*d2) ! Note: rlsymb automatically avoids plotting symbol out of axis range
				else if (iatom_on_plane_far==1) then
					call DUPLX
					call rlmess(trim(atmlabtext),n1*d1-movetext*pix2usr,n2*d2+movetext*pix2usr)
				end if
			end if
		end if
	end do
	call color("WHITE") !Restore to default
end if

!Draw vdW contour line (electron density=0.001)
if (idrawplanevdwctr==1) then
	call setcolor(ivdwclrindctr) !This routine must be invoked before LINWID, else no effect
	if (ivdwctrlabsize/=0) then  !Enable showing isovalue on contour lines
		call DISALF
		call height(ivdwctrlabsize)
		call labels('float','contur')
		call labdig(3,'contur')
	else
		call labels('NONE','contur')		
	end if
	if (vdwctrstyle(2)==0) nsizestyle=1
	call myline(vdwctrstyle,nsizestyle)
	CALL LINWID(iwidthvdwctr)
	call contur(xcoord,ngridnum1,ycoord,ngridnum2,planemattmp,0.001D0)
	CALL LINWID(1) !Restore to default
	call color("WHITE") !Restore to default
end if

!Draw all kinds of topology information on color-filled/contour/gradient/vector field graph
if (idrawtype==1.or.idrawtype==2.or.idrawtype==6.or.idrawtype==7) then	
10	continue
	!Draw topology paths on color-filled/contour/gradient/vector field graph. If iplaneoutall==1, then output the data points
	if (numpath>0.and.imarkpath==1) then
		if (iplaneoutall==0) then
			call setcolor(iclrpath)
			call HSYMBL(sizemarkpath)
		else if (iplaneoutall==1) then
			open(10,file="planepath.txt",status="replace")
		end if
		do ipath=1,numpath
			if ( plesel==1.and.any(abs(topopath(3,1:pathnumpt(ipath),ipath)*scll-orgz2D) > disshowlabel) ) cycle
			if ( plesel==2.and.any(abs(topopath(2,1:pathnumpt(ipath),ipath)*scll-orgy2D) > disshowlabel) ) cycle
			if ( plesel==3.and.any(abs(topopath(1,1:pathnumpt(ipath),ipath)*scll-orgx2D) > disshowlabel) ) cycle
			if ( plesel==4.or.plesel==5.or.plesel==6.or.plesel==7 ) then
				ioutplane=0
				do ipt=1,pathnumpt(ipath)
					if (potpledis(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,&
					topopath(1,ipt,ipath)*scll,topopath(2,ipt,ipath)*scll,topopath(3,ipt,ipath)*scll)>disshowlabel) then
						ioutplane=1
						exit
					end if
				end do
				if (ioutplane==1) cycle
			end if
			do ipt=1,pathnumpt(ipath)
				posmarkx=topopath(1,ipt,ipath)*scll
				posmarky=topopath(2,ipt,ipath)*scll
				posmarkz=topopath(3,ipt,ipath)*scll
				if (plesel==1) then
					if (posmarkx<init1.or.posmarkx>end1.or.posmarky<init2.or.posmarky>end2) cycle !To avoid path out of plotting range
					if (iplaneoutall==0) call rlsymb(21,posmarkx,posmarky)
					if (iplaneoutall==1) write(10,"(2f12.6)") posmarkx*b2a,posmarky*b2a
				else if (plesel==2) then
					if (posmarkx<init1.or.posmarkx>end1.or.posmarkz<init2.or.posmarkz>end2) cycle !To avoid path out of plotting range
					if (iplaneoutall==0) call rlsymb(21,posmarkx,posmarkz)
					if (iplaneoutall==1) write(10,"(2f12.6)") posmarkx*b2a,posmarkz*b2a
				else if (plesel==3) then
					if (posmarky<init1.or.posmarky>end1.or.posmarkz<init2.or.posmarkz>end2) cycle !To avoid path out of plotting range
					if (iplaneoutall==0) call rlsymb(21,posmarky,posmarkz)
					if (iplaneoutall==1) write(10,"(2f12.6)") posmarky*b2a,posmarkz*b2a
				else if (plesel==4.or.plesel==5.or.plesel==6.or.plesel==7) then
					call pointprjple(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,posmarkx,posmarky,posmarkz,prjx,prjy,prjz)
					if (abs(v1x*v2y-v2x*v1y)>1D-8) then
						det2_2=v1x*v2y-v2x*v1y
						n1=( (prjx-orgx2D)*v2y-v2x*(prjy-orgy2D) )/det2_2
						n2=( v1x*(prjy-orgy2D)-(prjx-orgx2D)*v1y )/det2_2
					else if (abs(v1x*v2z-v2x*v1z)>1D-8) then
						det2_2=v1x*v2z-v2x*v1z
						n1=( (prjx-orgx2D)*v2z-v2x*(prjz-orgz2D) )/det2_2
						n2=( v1x*(prjz-orgz2D)-(prjx-orgx2D)*v1z )/det2_2
					else if (abs(v1y*v2z-v2y*v1z)>1D-8) then
						det2_2=v1y*v2z-v2y*v1z
						n1=( (prjy-orgy2D)*v2z-v2y*(prjz-orgz2D) )/det2_2
						n2=( v1y*(prjz-orgz2D)-(prjy-orgy2D)*v1z )/det2_2
					end if
					tmp1=n1*d1; tmp2=n2*d2
					if (tmp1<init1.or.tmp1>end1.or.tmp2<init2.or.tmp2>end2) cycle !To avoid path out of plotting range
					if (iplaneoutall==0) call rlsymb(21,tmp1,tmp2)
					if (iplaneoutall==1) write(10,"(2f12.6)") n1*d1*b2a,n2*d2*b2a
				end if
			end do
			if (iplaneoutall==1) write(10,*) !Leave a blank line between each paths
		end do
		if (iplaneoutall==1) close(10)
	end if
	
	!Draw interbasin paths on color-filled/contour/gradient/vector field graph. If iplaneoutall==1, then output the data points
	if (nple3n1path>0.and.idrawintbasple==1) then
		if (iplaneoutall==0) then
			call setcolor(iclr3n1path)
			call HSYMBL(sizemark3n1path)
		else if (iplaneoutall==1) then
			open(10,file="planeinterbasin.txt",status="replace")
		end if
		do icp=1,nple3n1path
			do idir=1,2
				do ipt=1,n3n1plept
					posmarkx=ple3n1path(1,ipt,idir,icp)*scll
					posmarky=ple3n1path(2,ipt,idir,icp)*scll
					posmarkz=ple3n1path(3,ipt,idir,icp)*scll
					if (plesel==1) then
						if (abs(posmarkz-orgz2D) > disshowlabel) cycle
						if (posmarkx<init1.or.posmarkx>end1.or.posmarky<init2.or.posmarky>end2) cycle !To avoid path out of plotting range
						if (iplaneoutall==0) call rlsymb(21,posmarkx,posmarky)
						if (iplaneoutall==1) write(10,"(2f12.6)") posmarkx*b2a,posmarky*b2a
					else if (plesel==2) then
						if (abs(posmarky-orgy2D) > disshowlabel) cycle
						if (posmarkx<init1.or.posmarkx>end1.or.posmarkz<init2.or.posmarkz>end2) cycle !To avoid path out of plotting range
						if (iplaneoutall==0) call rlsymb(21,posmarkx,posmarkz)
						if (iplaneoutall==1) write(10,"(2f12.6)") posmarkx*b2a,posmarkz*b2a
					else if (plesel==3) then
						if (abs(posmarkx-orgx2D) > disshowlabel) cycle
						if (posmarky<init1.or.posmarky>end1.or.posmarkz<init2.or.posmarkz>end2) cycle !To avoid path out of plotting range
						if (iplaneoutall==0) call rlsymb(21,posmarky,posmarkz)
						if (iplaneoutall==1) write(10,"(2f12.6)") posmarky*b2a,posmarkz*b2a
					else if (plesel==4.or.plesel==5.or.plesel==6.or.plesel==7) then
						call pointprjple(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,posmarkx,posmarky,posmarkz,prjx,prjy,prjz)
						if ( (posmarkx-prjx)**2+(posmarky-prjy)**2+(posmarkz-prjz)**2 > disshowlabel**2) cycle
						if (abs(v1x*v2y-v2x*v1y)>1D-8) then
							det2_2=v1x*v2y-v2x*v1y
							n1=( (prjx-orgx2D)*v2y-v2x*(prjy-orgy2D) )/det2_2
							n2=( v1x*(prjy-orgy2D)-(prjx-orgx2D)*v1y )/det2_2
						else if (abs(v1x*v2z-v2x*v1z)>1D-8) then
							det2_2=v1x*v2z-v2x*v1z
							n1=( (prjx-orgx2D)*v2z-v2x*(prjz-orgz2D) )/det2_2
							n2=( v1x*(prjz-orgz2D)-(prjx-orgx2D)*v1z )/det2_2
						else if (abs(v1y*v2z-v2y*v1z)>1D-8) then
							det2_2=v1y*v2z-v2y*v1z
							n1=( (prjy-orgy2D)*v2z-v2y*(prjz-orgz2D) )/det2_2
							n2=( v1y*(prjz-orgz2D)-(prjy-orgy2D)*v1z )/det2_2
						end if
						tmp1=n1*d1; tmp2=n2*d2
						if (tmp1<init1.or.tmp1>end1.or.tmp2<init2.or.tmp2>end2) cycle !To avoid path out of plotting range
						if (iplaneoutall==0) call rlsymb(21,tmp1,tmp2)
						if (iplaneoutall==1) write(10,"(2f12.6)") n1*d1*b2a,n2*d2*b2a
					end if
				end do
				if (iplaneoutall==1) write(10,*) !Leave a blank line between each paths
			end do
		end do
		if (iplaneoutall==1) close(10)
	end if
	
	!Draw critical points on color-filled/contour/gradient/vector field graph. If iplaneoutall==1, then output the data points
	if (numcp>0) then
		if (iplaneoutall==0) then
			call HSYMBL(sizemarkcp)
		else if (iplaneoutall==1) then
			open(10,file="planeCP.txt",status="replace")
		end if
		do icp=1,numcp
			if (CPtype(icp)==1.and.imark3n3==0) cycle
			if (CPtype(icp)==2.and.imark3n1==0) cycle
			if (CPtype(icp)==3.and.imark3p1==0) cycle
			if (CPtype(icp)==4.and.imark3p3==0) cycle
			if (iplaneoutall==0) then
				if (CPtype(icp)==1) call setrgb(0.7D0,0.4D0,0.1D0)
				if (CPtype(icp)==2) call color("blue")
				if (CPtype(icp)==3) call color("orange")
				if (CPtype(icp)==4) call color("green")
			end if
			posmarkx=CPpos(1,icp)*scll
			posmarky=CPpos(2,icp)*scll
			posmarkz=CPpos(3,icp)*scll
			if (plesel==1.and.abs(posmarkz-orgz2D)<disshowlabel) then
				if (posmarkx<init1.or.posmarkx>end1.or.posmarky<init2.or.posmarky>end2) cycle !To avoid path out of plotting range
				if (iplaneoutall==0) call rlsymb(21,posmarkx,posmarky)
				if (iplaneoutall==1) write(10,"(2f12.6,i4)") posmarkx*b2a,posmarky*b2a,CPtype(icp)
			else if (plesel==2.and.abs(posmarky-orgy2D)<disshowlabel) then
				if (posmarkx<init1.or.posmarkx>end1.or.posmarkz<init2.or.posmarkz>end2) cycle !To avoid path out of plotting range
				if (iplaneoutall==0) call rlsymb(21,posmarkx,posmarkz)
				if (iplaneoutall==1) write(10,"(2f12.6,i4)") posmarkx*b2a,posmarkz*b2a,CPtype(icp)
			else if (plesel==3.and.abs(posmarkx-orgx2D)<disshowlabel) then
				if (posmarky<init1.or.posmarky>end1.or.posmarkz<init2.or.posmarkz>end2) cycle !To avoid path out of plotting range
				if (iplaneoutall==0) call rlsymb(21,posmarky,posmarkz)
				if (iplaneoutall==1) write(10,"(2f12.6,i4)") posmarky*b2a,posmarky*b2a,CPtype(icp)
			else if (plesel==4.or.plesel==5.or.plesel==6.or.plesel==7) then
				if (potpledis(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,posmarkx,posmarky,posmarkz)<disshowlabel) then
					call pointprjple(a1x,a1y,a1z,a2x,a2y,a2z,a3x,a3y,a3z,posmarkx,posmarky,posmarkz,prjx,prjy,prjz)
					if (abs(v1x*v2y-v2x*v1y)>1D-8) then
						det2_2=v1x*v2y-v2x*v1y
						n1=( (prjx-orgx2D)*v2y-v2x*(prjy-orgy2D) )/det2_2
						n2=( v1x*(prjy-orgy2D)-(prjx-orgx2D)*v1y )/det2_2
					else if (abs(v1x*v2z-v2x*v1z)>1D-8) then
						det2_2=v1x*v2z-v2x*v1z
						n1=( (prjx-orgx2D)*v2z-v2x*(prjz-orgz2D) )/det2_2
						n2=( v1x*(prjz-orgz2D)-(prjx-orgx2D)*v1z )/det2_2
					else if (abs(v1y*v2z-v2y*v1z)>1D-8) then
						det2_2=v1y*v2z-v2y*v1z
						n1=( (prjy-orgy2D)*v2z-v2y*(prjz-orgz2D) )/det2_2
						n2=( v1y*(prjz-orgz2D)-(prjy-orgy2D)*v1z )/det2_2
					end if
					tmp1=n1*d1; tmp2=n2*d2
					if (tmp1<init1.or.tmp1>end1.or.tmp2<init2.or.tmp2>end2) cycle !To avoid path out of plotting range
					if (iplaneoutall==0) call rlsymb(21,tmp1,tmp2)
					if (iplaneoutall==1) write(10,"(2f12.6,i4)") n1*d1*b2a,n2*d2*b2a,CPtype(icp)
				end if
			end if
		end do
		if (iplaneoutall==1) close(10)
	end if
	if (iplaneoutall==1) return
end if

!Relief map & shaded surface map with/without projection
if (idrawtype==3.or.idrawtype==4.or.idrawtype==5) then
	CALL AXSPOS(100,2800) !Make position of coordinate proper
	planetrunc=planemat
	!Now truncate the value in planemat to uplimit of Z-scale of relief map and save to planetrunc, else the color scale will range from
	!minimal to maximum, so the color transition is not completely between lower and upper limit of relief map, and effect is not good
	if (inucespplot==1) then !Now cut value, nuclear attraction potential is too big so treat it separately
		where (planetrunc>50) planetrunc=50
	else
! 		if (any(planetrunc>3).or.any(planetrunc<-3)) write(*,"(a)") " Note: The function values are temporarily truncated &
! 		at +/- 3.0 to garanteee that data range is completely within range of Z-axis"
		where (planetrunc>3)
			planetrunc=3
		elsewhere (planetrunc<-3)
			planetrunc=-3
		end where
	end if
	CALL AXSLEN(3000,3000) !Shouldn't smaller than xxx of page(xxx,xxx), else project map couldn't show completely
	call complx !A good font
	!!! Set axis
	call axis3D(2.0D0,(end2-init2)/(end1-init1)*2.0D0,2.0D0)
	if (idrawtype==5) then !Employ large negative part in Z to avoid relief map overlay the projected map
		if (inucespplot==1.and.ifiletype/=4) then !Nuclear ESP, since it is not from atomic charge (ifiletype==4), the value will be huge, use large Z uplimit
			CALL GRAF3D(init1,end1,init1-shiftx,planestpx, init2,end2,init2-shifty,planestpy, -50.0D0,50D0,-50.0D0,5.0D0)
		else !Common cases
			CALL GRAF3D(init1,end1,init1-shiftx,planestpx, init2,end2,init2-shifty,planestpy, -5.0D0,3.0D0,-5.0D0,1D0)
		end if
	else if (idrawtype==3.or.idrawtype==4) then
		if (inucespplot==1.and.ifiletype/=4) then !Nuclear ESP
			CALL GRAF3D(init1,end1,init1-shiftx,planestpx, init2,end2,init2-shifty,planestpy, -3.0D0,50.0D0,-3.0D0,5.0D0)
		else !Common cases
			CALL GRAF3D(init1,end1,init1-shiftx,planestpx, init2,end2,init2-shifty,planestpy, -3.0D0,3.0D0,-3.0D0,1D0)
		end if
	end if
	!!! Plot
	call light('on')
	if (idrawtype==3) then
! 		call surmat(planetrunc,ngridnum1,ngridnum2,1,1) !Works well for most functions, however not good for Laplacian
		call surmsh("only") !Draw grids on shaded surface
		CALL SHDMOD('SMOOTH','SURFACE')
		call surshd(xcoord,ngridnum1,ycoord,ngridnum2,planetrunc)
		call surmsh("OFF") !Recover to default, otherwise when drawing molecule structure later, black lines will present on object surfaces
	else if (idrawtype==4.or.idrawtype==5) then
		if (idrawtype==5) then !Draw projection
			CALL GRFINI(-1.0D0,-(end2-init2)/(end1-init1),-1.0D0, 1.0D0,-(end2-init2)/(end1-init1),-1.0D0, 1.0D0,(end2-init2)/(end1-init1),-1D0)
			call SETGRF('none','none','none','none')
			call AUTRES(ngridnum1,ngridnum2)
			call VKXBAR(170)
			call GRAF3(init1,end1,init1-shiftx,planestpx, init2,end2,init2-shifty,planestpy, init3,end3,init3-shiftz,planestpz)
			call CRVMAT(planetrunc,ngridnum1,ngridnum2,fillcoloritpx,fillcoloritpy)
			CALL GRFFIN
		end if
		call litmod(3,'on')
		call litmod(1,'on')
		call litpos(1,XVU,YVU,ZVU,'ANGLE')
		call surmsh(drawsurmesh) !Draw grids on shaded surface
		CALL SHDMOD('SMOOTH','SURFACE')
		call zscale(surcolorzmin,surcolorzmax) !For shaded relief map, set different color to shad defined by user
		call surshd(xcoord,ngridnum1,ycoord,ngridnum2,planetrunc)
		call surmsh("OFF") !Recover to default, otherwise when drawing molecule structure later, black lines will present on object surfaces
	end if
end if

call DISFIN

!Convert to original length unit
disshowlabel=disshowlabel/scll
if (ilenunit2D==2) call convgridlenunit(2)
end subroutine


end module



!---- Select color by index
subroutine selcolor(clrind)
integer clrind
write(*,*) "1  = Red        2  = Green"
write(*,*) "3  = Blue       4  = White"
write(*,*) "5  = Black      6  = Gray"
write(*,*) "7  = Cyan       8  = Yellow"
write(*,*) "9  = Orange     10 = Magenta"
write(*,*) "11 = Crimson    12 = Dark green"
write(*,*) "13 = Purple     14 = Brown"
write(*,*) "15 = Dark blue"
read(*,*) clrind
end subroutine

!---- Set color used by DISLIN routine by index
subroutine setcolor(clrind)
integer clrind
if (clrind==1) call color('RED')
if (clrind==2) call color('GREEN')
if (clrind==3) call color('BLUE')
if (clrind==4) call color('BLACK') !Due to current mode is REVERSE, so WHITE=BLACK
if (clrind==5) call color('WHITE')
if (clrind==6) call setRGB(0.65D0,0.65D0,0.7D0) !gray
if (clrind==7) call color('CYAN')
if (clrind==8) call color('YELLOW')
if (clrind==9) call color('ORANGE')
if (clrind==10) call color('MAGENTA')
if (clrind==11) call setRGB(0.7D0,0.0D0,0D0) !Crimson
if (clrind==12) call setRGB(0.0D0,0.7D0,0D0) !Dark green
if (clrind==13) call setRGB(0.4D0,0.0D0,0.84D0) !Purple
if (clrind==14) call setRGB(0.7D0,0.5D0,0.4D0) !Brown
if (clrind==15) call setRGB(0.0D0,0.0D0,0.5D0) !Dark blue
end subroutine

!Translate color index (defined by colorname) to R,G,B components
subroutine clridx2RGB(iclrind,Rcomp,Gcomp,Bcomp)
integer iclrind
real*8 Rcomp,Gcomp,Bcomp
Rcomp=0;Gcomp=0;Bcomp=0	!White
if (iclrind==1) then
	Rcomp=1
else if (iclrind==2) then
	Gcomp=1
else if (iclrind==3) then
	Bcomp=1
else if (iclrind==4) then
	Rcomp=1;Gcomp=1;Bcomp=1
else if (iclrind==5) then
	continue
else if (iclrind==6) then
	Rcomp=0.65D0;Gcomp=0.65D0;Bcomp=0.7D0
else if (iclrind==7) then
	Gcomp=1D0;Bcomp=1D0
else if (iclrind==8) then
	Rcomp=1D0;Gcomp=1D0
else if (iclrind==9) then
	Rcomp=1D0;Gcomp=0.5D0
else if (iclrind==10) then
	Rcomp=1D0;Bcomp=1D0
else if (iclrind==11) then
	Rcomp=0.7D0
else if (iclrind==12) then
	Gcomp=0.7D0
else if (iclrind==13) then
	Rcomp=0.4D0;Bcomp=0.84D0
else if (iclrind==14) then
	Rcomp=0.7D0;Gcomp=0.5D0;Bcomp=0.4D0
else if (iclrind==15) then
	Bcomp=0.5D0
end if
end subroutine
