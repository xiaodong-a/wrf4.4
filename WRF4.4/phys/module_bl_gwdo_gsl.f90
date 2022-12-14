




module module_bl_gwdo_gsl
contains

   subroutine gwdo_gsl(u3d,v3d,t3d,qv3d,p3d,p3di,pi3d,z,                       &
                  rublten,rvblten,rthblten,                                    &
                  dtaux3d_ls,dtauy3d_ls,dtaux3d_bl,dtauy3d_bl,                 &
                  dtaux3d_ss,dtauy3d_ss,dtaux3d_fd,dtauy3d_fd,                 &
                  dusfcg_ls,dvsfcg_ls,dusfcg_bl,dvsfcg_bl,dusfcg_ss,dvsfcg_ss, &
                  dusfcg_fd,dvsfcg_fd,xland,br,                                &
                  var2d,oc12d,oa2d1,oa2d2,oa2d3,oa2d4,ol2d1,ol2d2,ol2d3,ol2d4, &
                  var2dss,oc12dss,                                             &
                  oa2d1ss,oa2d2ss,oa2d3ss,oa2d4ss,                             &
                  ol2d1ss,ol2d2ss,ol2d3ss,ol2d4ss,                             &
                  sina,cosa,znu,znw,p_top,dz,pblh,                             &
                  cp,g,rd,rv,ep1,pi,                                           &
                  dt,dx,kpbl2d,itimestep,gwd_opt,gwd_diags,                    &
                  spp_pbl,pattern_spp_pbl,                                     &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte)

   implicit none















































  integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                 &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
  integer,  intent(in   )   ::      itimestep,gwd_opt,gwd_diags

  real,     intent(in   )   ::      dt,dx,cp,g,rd,rv,ep1,pi

  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(in   )   ::                                           qv3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                          t3d, &
                                                                            z, &
                                                                           dz
  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(in   )   ::                                           p3di

  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(inout)   ::                                        rublten, &
                                                                      rvblten, &
                                                                      rthblten
  real,     dimension( ims:ime, kms:kme, jms:jme ), optional                 , &
            intent(inout)   ::  dtaux3d_ls,dtauy3d_ls,dtaux3d_bl,dtauy3d_bl,   &
                                dtaux3d_ss,dtauy3d_ss,dtaux3d_fd,dtauy3d_fd

  real,     dimension( ims:ime, kms:kme) ::                                    &
                                  dtaux2d_ls,dtauy2d_ls,dtaux2d_bl,dtauy2d_bl, &
                                  dtaux2d_ss,dtauy2d_ss,dtaux2d_fd,dtauy2d_fd

  real,      dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d

  integer,   dimension( ims:ime, jms:jme )                                   , &
             intent(in  )   ::                                         kpbl2d

  real,   dimension( ims:ime, jms:jme )                                      , &
             intent(in  )   ::                                           pblh, & 
                                                                           br, &
                                                                        xland

  real,   dimension( ims:ime, jms:jme ), optional                            , &
             intent(inout  )   ::  dusfcg_ls,dvsfcg_ls,dusfcg_bl,dvsfcg_bl,    &
                                   dusfcg_ss,dvsfcg_ss,dusfcg_fd,dvsfcg_fd

  real,   dimension( ims:ime ) ::  dusfc_ls,dvsfc_ls,dusfc_bl,dvsfc_bl,        &
                                   dusfc_ss,dvsfc_ss,dusfc_fd,dvsfc_fd

  real,   dimension( ims:ime, jms:jme )                                      , &
             intent(in  )   ::                                          var2d, &
                                                                        oc12d, &
                                                      oa2d1,oa2d2,oa2d3,oa2d4, &
                                                      ol2d1,ol2d2,ol2d3,ol2d4, &
           
           
                    var2dss,oc12dss,                                           &
                    oa2d1ss,oa2d2ss,oa2d3ss,oa2d4ss,                           &
                    ol2d1ss,ol2d2ss,ol2d3ss,ol2d4ss,                           &
           
                    sina,cosa

  real,     dimension( kms:kme )                                             , &
            optional                                                         , &
            intent(in  )   ::                                             znu, &
                                                                          znw

  real,     optional, intent(in  )   ::                                 p_top


     INTEGER,  INTENT(IN)                                               ::spp_pbl                                                                         
     REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN),OPTIONAL  ::pattern_spp_pbl                                                                 
     REAL, DIMENSION(ITS:ITE)                         ::    rstoch1D




  real,   dimension( its:ite, kts:kte )  ::                           delprsi, &
                                                                          pdh
  real,     dimension( its:ite, kts:kte+1 )   ::                         pdhi
  real,   dimension( its:ite, 4 )        ::                               oa4, &
                                                                          ol4, &
                                                                          oa4ss, &
                                                                          ol4ss
  real,   dimension( its:ite, kts:kte ) :: ugeo, vgeo, dudt, dvdt
               
  integer ::  i,j,k,kdt,kpblmax

   do k = kts,kte
     if(znu(k).gt.0.6) kpblmax = k + 1
   enddo

   do j = jts,jte

      do k = kts,kte+1
         do i = its,ite
            if(k.le.kte)pdh(i,k) = p3d(i,k,j)
             pdhi(i,k) = p3di(i,k,j)
         enddo
      enddo

      do k = kts,kte
        do i = its,ite
          delprsi(i,k) = pdhi(i,k)-pdhi(i,k+1)

          ugeo(i,k) = u3d(i,k,j)*cosa(i,j) - v3d(i,k,j)*sina(i,j)
          vgeo(i,k) = u3d(i,k,j)*sina(i,j) + v3d(i,k,j)*cosa(i,j)
          dudt(i,k) = 0.0
          dvdt(i,k) = 0.0
        enddo
      enddo


      if (spp_pbl==1) then
         do i = its,ite
            rstoch1D(i)=pattern_spp_pbl(i,kts,j)
         enddo
      else
         do i = its,ite
            rstoch1D(i)=0.0
         enddo
      endif

      do i = its,ite
          oa4(i,1) = oa2d1(i,j)
          oa4(i,2) = oa2d2(i,j)
          oa4(i,3) = oa2d3(i,j)
          oa4(i,4) = oa2d4(i,j)
          ol4(i,1) = ol2d1(i,j)
          ol4(i,2) = ol2d2(i,j)
          ol4(i,3) = ol2d3(i,j)
          ol4(i,4) = ol2d4(i,j)
          oa4ss(i,1) = oa2d1ss(i,j)
          oa4ss(i,2) = oa2d2ss(i,j)
          oa4ss(i,3) = oa2d3ss(i,j)
          oa4ss(i,4) = oa2d4ss(i,j)
          ol4ss(i,1) = ol2d1ss(i,j)
          ol4ss(i,2) = ol2d2ss(i,j)
          ol4ss(i,3) = ol2d3ss(i,j)
          ol4ss(i,4) = ol2d4ss(i,j)
      enddo
      call gwdo2d(dudt=dudt(its,kts),dvdt=dvdt(its,kts)                        &
              ,dthdt=rthblten(ims,kms,j)                                       &
              ,dtaux2d_ls=dtaux2d_ls,dtauy2d_ls=dtauy2d_ls                     &
              ,dtaux2d_bl=dtaux2d_bl,dtauy2d_bl=dtauy2d_bl                     &
              ,dtaux2d_ss=dtaux2d_ss,dtauy2d_ss=dtauy2d_ss                     &
              ,dtaux2d_fd=dtaux2d_fd,dtauy2d_fd=dtauy2d_fd                     &
              ,u1=ugeo(its,kts),v1=vgeo(its,kts)                               &
              ,t1=t3d(ims,kms,j),q1=qv3d(ims,kms,j)                            &
              ,del=delprsi(its,kts)                                            &
              ,prsi=pdhi(its,kts)                                              &
              ,prsl=pdh(its,kts),prslk=pi3d(ims,kms,j)                         &
              ,zl=z(ims,kms,j),rcl=1.0                                         &
              ,xland1=xland(ims,j),br1=br(ims,j),hpbl=pblh(ims,j)              &
              ,dz2=dz(ims,kms,j)                                               &
              ,kpblmax=kpblmax                                                 &
              ,dusfc_ls=dusfc_ls,dvsfc_ls=dvsfc_ls                             &
              ,dusfc_bl=dusfc_bl,dvsfc_bl=dvsfc_bl                             &
              ,dusfc_ss=dusfc_ss,dvsfc_ss=dvsfc_ss                             &
              ,dusfc_fd=dusfc_fd,dvsfc_fd=dvsfc_fd                             &
              ,var=var2d(ims,j),oc1=oc12d(ims,j)                               &
              ,oa4=oa4,ol4=ol4                                                 &
              ,varss=var2dss(ims,j),oc1ss=oc12dss(ims,j)                       &
              ,oa4ss=oa4ss,ol4ss=ol4ss                                         &
              ,g=g,cp=cp,rd=rd,rv=rv,fv=ep1,pi=pi                              &
              ,dxmeter=dx,deltim=dt                                            &
              ,kpbl=kpbl2d(ims,j),kdt=itimestep,lat=j                          &
              ,rstoch=rstoch1d                                                 &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde               &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme               &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )

      do k = kts,kte
         do i = its,ite

           rublten(i,k,j) = rublten(i,k,j)+dudt(i,k)*cosa(i,j) + dvdt(i,k)*sina(i,j)
           rvblten(i,k,j) = rvblten(i,k,j)-dudt(i,k)*sina(i,j) + dvdt(i,k)*cosa(i,j)
         end do
      end do


      IF (gwd_diags == 1) then 
        do k = kts,kte
          do i = its,ite

             dtaux3d_ls(i,k,j)=dtaux2d_ls(i,k)*cosa(i,j)+dtauy2d_ls(i,k)*sina(i,j)
             dtaux3d_bl(i,k,j)=dtaux2d_bl(i,k)*cosa(i,j)+dtauy2d_bl(i,k)*sina(i,j)
             dtaux3d_ss(i,k,j)=dtaux2d_ss(i,k)*cosa(i,j)+dtauy2d_ss(i,k)*sina(i,j)
             dtaux3d_fd(i,k,j)=dtaux2d_fd(i,k)*cosa(i,j)+dtauy2d_fd(i,k)*sina(i,j)
             dtauy3d_ls(i,k,j)=-dtaux2d_ls(i,k)*sina(i,j)+dtauy2d_ls(i,k)*cosa(i,j)
             dtauy3d_bl(i,k,j)=-dtaux2d_bl(i,k)*sina(i,j)+dtauy2d_bl(i,k)*cosa(i,j)
             dtauy3d_ss(i,k,j)=-dtaux2d_ss(i,k)*sina(i,j)+dtauy2d_ss(i,k)*cosa(i,j)
             dtauy3d_fd(i,k,j)=-dtaux2d_fd(i,k)*sina(i,j)+dtauy2d_fd(i,k)*cosa(i,j)
          enddo
        enddo
        do i = its,ite

           dusfcg_ls(i,j)=dusfc_ls(i)*cosa(i,j)+dvsfc_ls(i)*sina(i,j)
           dusfcg_bl(i,j)=dusfc_bl(i)*cosa(i,j)+dvsfc_bl(i)*sina(i,j)
           dusfcg_ss(i,j)=dusfc_ss(i)*cosa(i,j)+dvsfc_ss(i)*sina(i,j)
           dusfcg_fd(i,j)=dusfc_fd(i)*cosa(i,j)+dvsfc_fd(i)*sina(i,j)
           dvsfcg_ls(i,j)=-dusfc_ls(i)*sina(i,j)+dvsfc_ls(i)*cosa(i,j)
           dvsfcg_bl(i,j)=-dusfc_bl(i)*sina(i,j)+dvsfc_bl(i)*cosa(i,j)
           dvsfcg_ss(i,j)=-dusfc_ss(i)*sina(i,j)+dvsfc_ss(i)*cosa(i,j)
           dvsfcg_fd(i,j)=-dusfc_fd(i)*sina(i,j)+dvsfc_fd(i)*cosa(i,j)
        enddo
      ENDIF

   enddo  

   end subroutine gwdo_gsl



   subroutine gwdo2d(dudt,dvdt,dthdt,dtaux2d_ls,dtauy2d_ls,                    &
                    dtaux2d_bl,dtauy2d_bl,dtaux2d_ss,dtauy2d_ss,               &
                    dtaux2d_fd,dtauy2d_fd,u1,v1,t1,q1,                         &
                    del,                                                       &
                    prsi,prsl,prslk,zl,rcl,                                    &
                    xland1,br1,hpbl,dz2,                                       &
                    kpblmax,dusfc_ls,dvsfc_ls,dusfc_bl,dvsfc_bl,               &
                    dusfc_ss,dvsfc_ss,dusfc_fd,dvsfc_fd,var,oc1,oa4,ol4,       &
                    varss,oc1ss,oa4ss,ol4ss,                                   &
                    g,cp,rd,rv,fv,pi,dxmeter,deltim,kpbl,kdt,lat,rstoch,       &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte)






























































   implicit none

   integer              ::  kdt,lat,latd,lond,kpblmax,                         &
                            ids,ide, jds,jde, kds,kde,                         &
                            ims,ime, jms,jme, kms,kme,                         &
                            its,ite, jts,jte, kts,kte

   real                 ::  g,rd,rv,fv,cp,pi,dxmeter,deltim,rcl
   real                 ::  dudt(its:ite,kts:kte),dvdt(its:ite,kts:kte),       &
                            dthdt(ims:ime,kms:kme),                            &
                            dtaux2d_ls(ims:ime,kms:kme),dtauy2d_ls(ims:ime,kms:kme), &
                            dtaux2d_bl(ims:ime,kms:kme),dtauy2d_bl(ims:ime,kms:kme), &
                            dtaux2d_ss(ims:ime,kms:kme),dtauy2d_ss(ims:ime,kms:kme), &
                            dtaux2d_fd(ims:ime,kms:kme),dtauy2d_fd(ims:ime,kms:kme), &
                            t1(ims:ime,kms:kme),q1(ims:ime,kms:kme),           &
                            zl(ims:ime,kms:kme),prsl(its:ite,kts:kte),         &
                            prslk(ims:ime,kms:kme)
   real                 ::  u1(its:ite,kts:kte),v1(its:ite,kts:kte)
   real                 ::  prsi(its:ite,kts:kte+1),del(its:ite,kts:kte)
   real                 ::  oa4(its:ite,4),ol4(its:ite,4),                     &
                            oa4ss(its:ite,4),ol4ss(its:ite,4)




integer, parameter ::                                                          &
   gsl_gwd_ls      = 1,       & 
   gsl_gwd_bl      = 1,       & 
   gsl_gwd_ss      = 1,       & 
   gsl_gwd_fd      = 1,       & 
   gsl_diss_ht_opt = 0


   real, dimension(its:ite,kts:kte)     :: utendwave,vtendwave,thx,thvx,za
   real, dimension(ims:ime), intent(in) :: br1,hpbl,xland1
   real, dimension(its:ite)             :: govrth
   real, dimension(ims:ime,kms:kme), intent(in) :: dz2
   real, dimension(its:ite,kts:kte+1)   :: zq
   real                                 :: tauwavex0,tauwavey0,XNBV,density,   &
                                           tvcon,hpbl2
   integer                              :: kpbl2,kvar

   real, dimension(its:ite)             :: rstoch
   real                                 :: var_stoch(ims:ime),                 &
                                           varss_stoch(ims:ime)

   integer              ::  kpbl(ims:ime)
   real                 ::  var(ims:ime),oc1(ims:ime),                         &
                            varss(ims:ime),oc1ss(ims:ime),                     &
                            dusfc_ls(ims:ime),dvsfc_ls(ims:ime),               &
                            dusfc_bl(ims:ime),dvsfc_bl(ims:ime),               &
                            dusfc_ss(ims:ime),dvsfc_ss(ims:ime),               &
                            dusfc_fd(ims:ime),dvsfc_fd(ims:ime)


   real, parameter   :: dxmin_ss = 1000., dxmax_ss = 12000.  

   real, parameter   :: dxmin_ls = 3000., dxmax_ls = 13000.  
   real              :: ss_taper, ls_taper  




   real, dimension(its:ite,kts:kte)     :: utendform,vtendform
   real                 :: a1,a2,wsp
   real                 :: H_efold


   real,parameter       ::  ric     = 0.25  

   real,parameter       ::  dw2min  = 1.
   real,parameter       ::  rimin   = -100.
   real,parameter       ::  bnv2min = 1.0e-5
   real,parameter       ::  efmin   = 0.0
   real,parameter       ::  efmax   = 10.0
   real,parameter       ::  xl      = 4.0e4  
   real,parameter       ::  critac  = 1.0e-5
   real,parameter       ::  gmax    = 1.    
   real,parameter       ::  veleps  = 1.0                                                 
   real,parameter       ::  factop  = 0.5                                                  
   real,parameter       ::  frc     = 1.0      
   real,parameter       ::  ce      = 0.8     
   real,parameter       ::  cg      = 0.5    
   integer,parameter    ::  kpblmin = 2



   real, parameter      :: varmax_ss = 35.,   &
                           varmax_fd = 160.,  &
                           beta_ss = 0.1,     &
                           beta_fd = 0.2
   real                 :: var_temp
   real, dimension(its:ite) ::                &
                           varmax_ss_stoch,   &
                           varmax_fd_stoch



   integer              ::  i,k,lcap,lcapp1,nwd,idir,                          &
                            klcap,kp1,ikount,kk

   real                 ::  rcs,rclcs,csg,fdir,cleff,cleff_ss,cs,rcsks,        &
                            wdir,ti,rdz,temp,tem2,dw2,shr2,bvf2,rdelks,        &
                            wtkbj,tem,gfobnv,hd,fro,rim,temc,tem1,efact,       &
                            temv,dtaux,dtauy,eng0,eng1

   logical              ::  ldrag(its:ite),icrilv(its:ite),                    &
                            flag(its:ite),kloop1(its:ite)

   real                 ::  taub(its:ite),taup(its:ite,kts:kte+1),             &
                            xn(its:ite),yn(its:ite),                           &
                            ubar(its:ite),vbar(its:ite),                       &
                            fr(its:ite),ulow(its:ite),                         &
                            rulow(its:ite),bnv(its:ite),                       &
                            oa(its:ite),ol(its:ite),                           &
                            oass(its:ite),olss(its:ite),                       &
                            roll(its:ite),dtfac(its:ite),                      &
                            brvf(its:ite),xlinv(its:ite),                      &
                            delks(its:ite),delks1(its:ite),                    &
                            bnv2(its:ite,kts:kte),usqj(its:ite,kts:kte),       &
                            taud_ls(its:ite,kts:kte),taud_bl(its:ite,kts:kte), &
                            ro(its:ite,kts:kte),                               &
                            vtk(its:ite,kts:kte),vtj(its:ite,kts:kte),         &
                            zlowtop(its:ite),velco(its:ite,kts:kte-1),         &
                            coefm(its:ite),coefm_ss(its:ite)

   integer              ::  kbl(its:ite),klowtop(its:ite)

   logical :: iope
   integer,parameter    ::  mdir=8
   integer              ::  nwdir(mdir)
   data nwdir/6,7,5,8,2,3,1,4/



   real,parameter       :: frmax  = 10.
   real,parameter       :: olmin  = 1.0e-5
   real,parameter       :: odmin  = 0.1 
   real,parameter       :: odmax  = 10. 
   integer              :: komax(its:ite)
   integer              :: kblk
   real                 :: cd
   real                 :: zblk,tautem
   real                 :: pe,ke 
   real                 :: delx,dely,dxy4(4),dxy4p(4)
   real                 :: dxy(its:ite),dxyp(its:ite)
   real                 :: ol4p(4),olp(its:ite),od(its:ite)
   real                 :: taufb(its:ite,kts:kte+1)




   real, parameter ::      &
      clf_coeff = 3.4E+07, &  
                              
      clf_coeff_ss = 0.1, &        
      a1_coeff = 0.00026615161, &  
      a2_coeff = 0.005363, &       
      TOFD_coeff = 0.0759, &       
      Hefold_nom = 1500.           





   rcs    = sqrt(rcl)                                                   
   cs     = 1. / sqrt(rcl)                                                     
   csg    = cs * g                                                      
   lcap   = kte                                                         
   lcapp1 = lcap + 1                                                 
   fdir   = mdir / (2.0*pi)



if ( dxmeter .ge. dxmax_ls ) then
   ls_taper = 1.
else
   if ( dxmeter .le. dxmin_ls) then
      ls_taper = 0.
   else
      ls_taper = 0.5 * ( SIN(pi*(dxmeter-0.5*(dxmax_ls+dxmin_ls))/    &
                                (dxmax_ls-dxmin_ls)) + 1. )
   end if
end if
if ( dxmeter .ge. dxmax_ss ) then
   ss_taper = 1.
else
   if ( dxmeter .le. dxmin_ss) then
      ss_taper = 0.
   else
      ss_taper = dxmax_ss * (1. - dxmin_ss/dxmeter)/(dxmax_ss-dxmin_ss)
   end if
end if



   delx   = dxmeter 
   dely   = dxmeter
   dxy4(1)  = delx
   dxy4(2)  = dely
   dxy4(3)  = sqrt(delx*delx + dely*dely)
   dxy4(4)  = dxy4(3)
   dxy4p(1) = dxy4(2)
   dxy4p(2) = dxy4(1)
   dxy4p(3) = dxy4(4)
   dxy4p(4) = dxy4(3)




   dtaux = 0.0
   dtauy = 0.0
   do i = its,ite                                                       
     klowtop(i)    = 0
     kbl(i)        = 0
   enddo                                                             

   do i = its,ite                                                       
     xn(i)         = 0.0
     yn(i)         = 0.0
     ubar (i)      = 0.0
     vbar (i)      = 0.0
     roll (i)      = 0.0
     taub (i)      = 0.0
     oa(i)         = 0.0
     ol(i)         = 0.0
     oass(i)       = 0.0
     olss(i)       = 0.0
     ulow (i)      = 0.0
     dtfac(i)      = 1.0
     ldrag(i)      = .false.
     icrilv(i)     = .false. 
     flag(i)       = .true.
   enddo                                                             

   do k = kts,kte
     do i = its,ite
       usqj(i,k) = 0.0
       bnv2(i,k) = 0.0
       vtj(i,k)  = 0.0
       vtk(i,k)  = 0.0
       taup(i,k) = 0.0
       taud_ls(i,k) = 0.0
       taud_bl(i,k) = 0.0
       dtaux2d_ls(i,k)= 0.0
       dtauy2d_ls(i,k)= 0.0
       dtaux2d_bl(i,k)= 0.0
       dtauy2d_bl(i,k)= 0.0
       dtaux2d_ss(i,k)= 0.0
       dtauy2d_ss(i,k)= 0.0
       dtaux2d_fd(i,k)= 0.0
       dtauy2d_fd(i,k)= 0.0
     enddo
   enddo

   do i = its,ite
     dusfc_ls(i) = 0.0
     dvsfc_ls(i) = 0.0
     dusfc_bl(i) = 0.0
     dvsfc_bl(i) = 0.0
     dusfc_ss(i) = 0.0
     dvsfc_ss(i) = 0.0
     dusfc_fd(i) = 0.0
     dvsfc_fd(i) = 0.0
   enddo

   do i = its,ite
     taup(i,kte+1) = 0.0
     xlinv(i)     = 1.0/xl                                                   
   enddo


   do i = its,ite
     var_stoch(i)   = var(i)   + var(i)*0.666*rstoch(i)
     varss_stoch(i) = varss(i) + varss(i)*0.666*rstoch(i)
     varmax_ss_stoch(i) = varmax_ss + varmax_ss*0.5*rstoch(i)
     varmax_fd_stoch(i) = varmax_fd + varmax_fd*0.5*rstoch(i)
   enddo



   taufb(its:ite,kts:kte+1) = 0.0
   komax(its:ite) = 0

   do k = kts,kte
     do i = its,ite
       vtj(i,k)  = t1(i,k)  * (1.+fv*q1(i,k))
       vtk(i,k)  = vtj(i,k) / prslk(i,k)
       ro(i,k)   = 1./rd * prsl(i,k) / vtj(i,k) 
     enddo
   enddo



   do i = its,ite
     zlowtop(i) = 2. * var_stoch(i)
   enddo

   do i = its,ite
     kloop1(i) = .true.
   enddo

   do k = kts+1,kte
     do i = its,ite
       if(kloop1(i).and.zl(i,k)-zl(i,1).ge.zlowtop(i)) then
         klowtop(i) = k+1
         kloop1(i)  = .false.
       endif
     enddo
   enddo

   do i = its,ite
     kbl(i)   = max(kpbl(i), klowtop(i))
     kbl(i)   = max(min(kbl(i),kpblmax),kpblmin)
   enddo



   
   komax(:) = klowtop(:) - 1    

   do i = its,ite
     delks(i)  = 1.0 / (prsi(i,1) - prsi(i,kbl(i)))
     delks1(i) = 1.0 / (prsl(i,1) - prsl(i,kbl(i)))
   enddo



   do k = kts,kpblmax
     do i = its,ite
       if (k.lt.kbl(i)) then
         rcsks   = rcs     * del(i,k) * delks(i)
         rdelks  = del(i,k)  * delks(i)
         ubar(i) = ubar(i) + rcsks  * u1(i,k)      
         vbar(i) = vbar(i) + rcsks  * v1(i,k)      
         roll(i) = roll(i) + rdelks * ro(i,k)      
       endif
     enddo
   enddo






   do i = its,ite                                                       
     wdir   = atan2(ubar(i),vbar(i)) + pi
     idir   = mod(nint(fdir*wdir),mdir) + 1
     nwd    = nwdir(idir)
     oa(i)  = (1-2*int( (nwd-1)/4 )) * oa4(i,mod(nwd-1,4)+1)
     ol(i) = ol4(i,mod(nwd-1,4)+1) 
     
     oass(i)  = (1-2*int( (nwd-1)/4 )) * oa4ss(i,mod(nwd-1,4)+1)
     olss(i) = ol4ss(i,mod(nwd-1,4)+1) 





     ol4p(1) = ol4(i,2)
     ol4p(2) = ol4(i,1)
     ol4p(3) = ol4(i,4)
     ol4p(4) = ol4(i,3)
     olp(i)  = ol4p(mod(nwd-1,4)+1) 



     od(i) = olp(i)/max(ol(i),olmin)
     od(i) = min(od(i),odmax)
     od(i) = max(od(i),odmin)



     dxy(i)  = dxy4(MOD(nwd-1,4)+1)
     dxyp(i) = dxy4p(MOD(nwd-1,4)+1)
   enddo



IF ( ((gsl_gwd_ls .EQ. 1).or.(gsl_gwd_bl .EQ. 1)).and.   &
               (ls_taper .GT. 1.E-02) ) THEN   



   do k = kts,kte-1                                                     
     do i = its,ite                                                     
       ti        = 2.0 / (t1(i,k)+t1(i,k+1))                                
       rdz       = 1./(zl(i,k+1) - zl(i,k))
       tem1      = u1(i,k) - u1(i,k+1)
       tem2      = v1(i,k) - v1(i,k+1)   
       dw2       = rcl*(tem1*tem1 + tem2*tem2)
       shr2      = max(dw2,dw2min) * rdz * rdz
       bvf2      = g*(g/cp+rdz*(vtj(i,k+1)-vtj(i,k))) * ti                
       usqj(i,k) = max(bvf2/shr2,rimin)                            
       bnv2(i,k) = 2.0*g*rdz*(vtk(i,k+1)-vtk(i,k))/(vtk(i,k+1)+vtk(i,k))
       bnv2(i,k) = max( bnv2(i,k), bnv2min )
     enddo                                                          
   enddo                                                             



   do i = its,ite                                                       
     ulow(i) = max(sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i)), 1.0)
     rulow(i) = 1./ulow(i)
   enddo                                                             

   do k = kts,kte-1                                                    
     do i = its,ite                                                   
       velco(i,k)  = (0.5*rcs) * ((u1(i,k)+u1(i,k+1)) * ubar(i)                &
                                + (v1(i,k)+v1(i,k+1)) * vbar(i))                 
       velco(i,k)  = velco(i,k) * rulow(i)                               
       if ((velco(i,k).lt.veleps) .and. (velco(i,k).gt.0.)) then
         velco(i,k) = veleps                                      
       endif
     enddo                                                          
   enddo                                                             



   do i = its,ite                                                       
     ldrag(i) = velco(i,1).le.0.                                    
   enddo                                                             



   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) ldrag(i) = ldrag(i).or. velco(i,k).le.0.
     enddo                                                          
   enddo                                                             



   do k = kts,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) ldrag(i) = ldrag(i).or. bnv2(i,k).lt.0.
     enddo                                                          
   enddo                                                             






   do i = its,ite                                                       
     wtkbj     = (prsl(i,1)-prsl(i,2)) * delks1(i)
     bnv2(i,1) = wtkbj * bnv2(i,1)                                
     usqj(i,1) = wtkbj * usqj(i,1)                                
   enddo                                                             

   do k = kpblmin,kpblmax                                                
     do i = its,ite                                                    
       if (k .lt. kbl(i)) then
         rdelks    = (prsl(i,k)-prsl(i,k+1)) * delks1(i)
         bnv2(i,1) = bnv2(i,1) + bnv2(i,k) * rdelks
         usqj(i,1) = usqj(i,1) + usqj(i,k) * rdelks
       endif
     enddo                                                          
   enddo                                                             

   do i = its,ite                                                       
     ldrag(i) = ldrag(i) .or. bnv2(i,1).le.0.0                         
     ldrag(i) = ldrag(i) .or. ulow(i).eq.1.0                           
     ldrag(i) = ldrag(i) .or. var_stoch(i) .le. 0.0
   enddo



   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) usqj(i,k) = usqj(i,1)
     enddo                                                          
   enddo                                                             

   do i = its,ite 
     if (.not.ldrag(i))   then   
       bnv(i) = sqrt( bnv2(i,1) )
       fr(i) = bnv(i)  * rulow(i) * 2. * var_stoch(i) * od(i)
       fr(i) = min(fr(i),frmax)
       xn(i)  = ubar(i) * rulow(i)
       yn(i)  = vbar(i) * rulow(i)
     endif
   enddo






   do i = its,ite                                                       
     if (.not. ldrag(i))   then   
       efact    = (oa(i) + 2.) ** (ce*fr(i)/frc)                         
       efact    = min( max(efact,efmin), efmax )                            

       
       
       
       
       cleff = clf_coeff / sqrt(dxmeter)
       coefm(i) = (1. + ol(i)) ** (oa(i)+1.)                   
       xlinv(i) = coefm(i) / cleff                                             
       tem      = fr(i) * fr(i) * oc1(i)
       gfobnv   = gmax * tem / ((tem + cg)*bnv(i))   
       if ( gsl_gwd_ls .NE. 0 ) then
          taub(i)  = xlinv(i) * roll(i) * ulow(i) * ulow(i)                       &
                   * ulow(i) * gfobnv * efact          
       else     
          taub(i) = 0.0
       end if
     else                                                          
       taub(i) = 0.0                                             
       xn(i)   = 0.0                                             
       yn(i)   = 0.0                                             
     endif                                                         
   enddo

ENDIF   



  XNBV=0.
  tauwavex0=0.
  tauwavey0=0.
  density=1.2
  utendwave=0.
  vtendwave=0.
  zq=0.

  IF ( (gsl_gwd_ss .EQ. 1).and.(ss_taper.GT.1.E-02) ) THEN



    do k = kts,kte
      do i = its,ite
        thx(i,k) = t1(i,k)/prslk(i,k)
      enddo
    enddo

    do k = kts,kte
      do i = its,ite
        tvcon = (1.+fv*q1(i,k))
        thvx(i,k) = thx(i,k)*tvcon
      enddo
    enddo



    do k = kts,kte
      do i = its,ite
        zq(i,k+1) = dz2(i,k)+zq(i,k)
      enddo
    enddo

    do k = kts,kte
      do i = its,ite
        za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
      enddo
    enddo

    do i=its,ite
       hpbl2 = hpbl(i)+10.
       kpbl2 = kpbl(i)
       kvar = 1
       do k=kts+1,MAX(kpbl(i),kts+1)
          IF (za(i,k)>300.) then
             kpbl2 = k
             IF (k == kpbl(i)) then
                hpbl2 = hpbl(i)+10.
             ELSE
                hpbl2 = za(i,k)+10.
             ENDIF
             exit
          ENDIF
       enddo
       if((xland1(i)-1.5).le.0. .and. 2.*varss_stoch(i).le.hpbl(i))then
          if(br1(i).gt.0. .and. thvx(i,kpbl2)-thvx(i,kts) > 0.)then
            cleff_ss    = sqrt(dxy(i)**2 + dxyp(i)**2)
            cleff_ss    = clf_coeff_ss * max(dxmax_ss,cleff_ss)
            coefm_ss(i) = (1. + olss(i)) ** (oass(i)+1.)
            xlinv(i) = coefm_ss(i) / cleff_ss
            govrth(i)=g/(0.5*(thvx(i,kpbl2)+thvx(i,kts)))
            XNBV=sqrt(govrth(i)*(thvx(i,kpbl2)-thvx(i,kts))/hpbl2)

            if(abs(XNBV/u1(i,kpbl2)).gt.xlinv(i))then
              var_temp = MIN(varss_stoch(i),varmax_ss_stoch(i)) +                       &
                            MAX(0.,beta_ss*(varss_stoch(i)-varmax_ss_stoch(i)))
              tauwavex0=0.5*XNBV*xlinv(i)*(2.*var_temp)**2*ro(i,kvar)*u1(i,kvar)
              tauwavex0=tauwavex0*ss_taper   
            else
              tauwavex0=0.
            endif

            if(abs(XNBV/v1(i,kpbl2)).gt.xlinv(i))then
              var_temp = MIN(varss_stoch(i),varmax_ss_stoch(i)) +                       &
                            MAX(0.,beta_ss*(varss_stoch(i)-varmax_ss_stoch(i)))
              tauwavey0=0.5*XNBV*xlinv(i)*(2.*var_temp)**2*ro(i,kvar)*v1(i,kvar)
              tauwavey0=tauwavey0*ss_taper   
            else
              tauwavey0=0.
            endif

            do k=kts,kpbl(i) 
              utendwave(i,k)=-1.*tauwavex0*2.*max((1.-za(i,k)/hpbl2),0.)/hpbl2
              vtendwave(i,k)=-1.*tauwavey0*2.*max((1.-za(i,k)/hpbl2),0.)/hpbl2
            enddo
          endif
       endif
    enddo 

    do k = kts,kte
       do i = its,ite
         dudt(i,k)  = dudt(i,k) + utendwave(i,k)
         dvdt(i,k)  = dvdt(i,k) + vtendwave(i,k)
         dtaux2d_ss(i,k) = utendwave(i,k)
         dtauy2d_ss(i,k) = vtendwave(i,k)
         dusfc_ss(i) = dusfc_ss(i) + utendwave(i,k) * del(i,k)
         dvsfc_ss(i) = dvsfc_ss(i) + vtendwave(i,k) * del(i,k)
       enddo
    enddo

ENDIF  



IF ( (gsl_gwd_fd .EQ. 1).and.(ss_taper.GT.1.E-02) ) THEN

   utendform=0.
   vtendform=0.
   zq=0.

   IF ( (gsl_gwd_ss .NE. 1).and.(ss_taper.GT.1.E-02) ) THEN
      
      do k = kts,kte
        do i = its,ite
          zq(i,k+1) = dz2(i,k)+zq(i,k)
        enddo
      enddo

      do k = kts,kte
        do i = its,ite
          za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
        enddo
      enddo
   ENDIF

   DO i=its,ite
      IF ((xland1(i)-1.5) .le. 0.) then
          var_temp = MIN(varss_stoch(i),varmax_fd_stoch(i)) +                                   &
                        MAX(0.,beta_fd*(varss_stoch(i)-varmax_fd_stoch(i)))
         a1=a1_coeff*var_temp**2
         a2=a1*a2_coeff
         
         H_efold = max(2*varss_stoch(i),hpbl(i))
         H_efold = min(H_efold,Hefold_nom)
         DO k=kts,kte
            wsp=SQRT(u1(i,k)**2 + v1(i,k)**2)
            
            utendform(i,k)=-TOFD_coeff*wsp*u1(i,k)* &
                           EXP(-(za(i,k)/H_efold)**1.5)*a2*za(i,k)**(-1.2)*ss_taper
            vtendform(i,k)=-TOFD_coeff*wsp*v1(i,k)* &
                           EXP(-(za(i,k)/H_efold)**1.5)*a2*za(i,k)**(-1.2)*ss_taper
         ENDDO
      ENDIF
   ENDDO

   do k = kts,kte
      do i = its,ite
         dudt(i,k)  = dudt(i,k) + utendform(i,k)
         dvdt(i,k)  = dvdt(i,k) + vtendform(i,k)
         dtaux2d_fd(i,k) = utendform(i,k)
         dtauy2d_fd(i,k) = vtendform(i,k)
         dusfc_fd(i) = dusfc_fd(i) + utendform(i,k) * del(i,k)
         dvsfc_fd(i) = dvsfc_fd(i) + vtendform(i,k) * del(i,k)
      enddo
   enddo

ENDIF  


IF ( (gsl_gwd_ls .EQ. 1).and.(ls_taper.GT.1.E-02) ) THEN



   do k = kts,kpblmax
      do i = its,ite
         if (k .le. kbl(i)) taup(i,k) = taub(i)
      enddo
   enddo

   do k = kpblmin, kte-1                   
      kp1 = k + 1
      do i = its,ite





         if (k .ge. kbl(i)) then
           icrilv(i) = icrilv(i) .or. ( usqj(i,k) .lt. ric)                  &
                                 .or. (velco(i,k) .le. 0.0)
           brvf(i)  = max(bnv2(i,k),bnv2min) 
           brvf(i)  = sqrt(brvf(i))          
         endif
      enddo

      do i = its,ite
        if (k .ge. kbl(i) .and. (.not. ldrag(i)))   then   
          if (.not.icrilv(i) .and. taup(i,k) .gt. 0.0 ) then
            temv = 1.0 / velco(i,k)
            tem1 = coefm(i)/dxy(i)*(ro(i,kp1)+ro(i,k))*brvf(i)*velco(i,k)*0.5
            hd   = sqrt(taup(i,k) / tem1)
            fro  = brvf(i) * hd * temv



            tem2   = sqrt(usqj(i,k))
            tem    = 1. + tem2 * fro
            rim    = usqj(i,k) * (1.-fro) / (tem * tem)




            if (rim .le. ric) then  
              if ((oa(i) .le. 0.).or.(kp1 .ge. kpblmin )) then
                temc = 2.0 + 1.0 / tem2
                hd   = velco(i,k) * (2.*sqrt(temc)-temc) / brvf(i)
                taup(i,kp1) = tem1 * hd * hd
              endif
            else                    
              taup(i,kp1) = taup(i,k)
            endif
          endif
        endif
      enddo      
   enddo

   if(lcap.lt.kte) then                                               
      do klcap = lcapp1,kte                                          
         do i = its,ite                                                 
           taup(i,klcap) = prsi(i,klcap) / prsi(i,lcap) * taup(i,lcap)      
         enddo                                                       
      enddo                                                          
   endif      

ENDIF 




IF ( (gsl_gwd_bl .EQ. 1) .and. (ls_taper .GT. 1.E-02) ) THEN
                                                       
   do i = its,ite
      if(.not.ldrag(i)) then



        kblk = 0
        pe = 0.0
        do k = kte, kpblmin, -1
          if(kblk.eq.0 .and. k.le.komax(i)) then
            pe = pe + bnv2(i,k)*(zl(i,komax(i))-zl(i,k))*del(i,k)/g/ro(i,k)
            ke = 0.5*((rcs*u1(i,k))**2.+(rcs*v1(i,k))**2.)



            if(pe.ge.ke) then
              kblk = k
              kblk = min(kblk,kbl(i))
              zblk = zl(i,kblk)-zl(i,kts)
            endif
          endif
        enddo
        if(kblk.ne.0) then



          cd = max(2.0-1.0/od(i),0.0)
          taufb(i,kts) = 0.5 * roll(i) * coefm(i) / max(dxmax_ls,dxy(i))**2 * cd * dxyp(i)   &
                         * olp(i) * zblk * ulow(i)**2
          tautem = taufb(i,kts)/float(kblk-kts)
          do k = kts+1, kblk
            taufb(i,k) = taufb(i,k-1) - tautem
          enddo



          
        endif
      endif
   enddo

ENDIF   

IF ( (gsl_gwd_ls .EQ. 1 .OR. gsl_gwd_bl .EQ. 1) .and. (ls_taper .GT. 1.E-02) ) THEN



   do k = kts,kte                                                       
     do i = its,ite                                                       
       taud_ls(i,k) = 1. * (taup(i,k+1) - taup(i,k)) * csg / del(i,k)
       taud_bl(i,k) = 1. * (taufb(i,k+1) - taufb(i,k)) * csg / del(i,k)
     enddo                                                             
   enddo                                                             




   do klcap = lcap,kte                                               
     do i = its,ite                                                    
       taud_ls(i,klcap) = taud_ls(i,klcap) * factop
       taud_bl(i,klcap) = taud_bl(i,klcap) * factop
     enddo                                                          
   enddo                                                             





   do k = kts,kpblmax-1
      do i = its,ite                                                    
         if (k .le. kbl(i)) then
           if((taud_ls(i,k)+taud_bl(i,k)).ne.0.)                         &
              dtfac(i) = min(dtfac(i),abs(velco(i,k)                     &
                   /(deltim*rcs*(taud_ls(i,k)+taud_bl(i,k)))))
         endif
      enddo
   enddo

   do k = kts,kte                                                       
      do i = its,ite 
         taud_ls(i,k)  = taud_ls(i,k) * dtfac(i) * ls_taper
         taud_bl(i,k)  = taud_bl(i,k) * dtfac(i) * ls_taper
         
         
         dtaux2d_ls(i,k) = taud_ls(i,k) * xn(i)
         dtauy2d_ls(i,k) = taud_ls(i,k) * yn(i)
         dtaux2d_bl(i,k) = taud_bl(i,k) * xn(i)
         dtauy2d_bl(i,k) = taud_bl(i,k) * yn(i)
         dudt(i,k)  = dtaux2d_ls(i,k) + dtaux2d_bl(i,k) + dudt(i,k)
         dvdt(i,k)  = dtauy2d_ls(i,k) + dtauy2d_bl(i,k) + dvdt(i,k)
         dusfc_ls(i)  = dusfc_ls(i) + dtaux2d_ls(i,k) * del(i,k)
         dvsfc_ls(i)  = dvsfc_ls(i) + dtauy2d_ls(i,k) * del(i,k)
         dusfc_bl(i)  = dusfc_bl(i) + dtaux2d_bl(i,k) * del(i,k)
         dvsfc_bl(i)  = dvsfc_bl(i) + dtauy2d_bl(i,k) * del(i,k)
         if ( gsl_diss_ht_opt .EQ. 1 ) then
            
            
            eng0 = 0.5*( (rcs*u1(i,k))**2. + (rcs*v1(i,k))**2. )
            
            eng1 = 0.5*( (rcs*(u1(i,k)+(dtaux2d_ls(i,k)+dtaux2d_bl(i,k))*deltim))**2. + &
                         (rcs*(v1(i,k)+(dtauy2d_ls(i,k)+dtauy2d_bl(i,k))*deltim))**2. )
            
            dthdt(i,k) = dthdt(i,k) + max((eng0-eng1),0.0)/cp/deltim/prslk(i,k)
         end if
      enddo                                                          
   enddo

ENDIF                                                             


do i = its,ite
   dusfc_ls(i) = (-1./g*rcs) * dusfc_ls(i)
   dvsfc_ls(i) = (-1./g*rcs) * dvsfc_ls(i)
   dusfc_bl(i) = (-1./g*rcs) * dusfc_bl(i)
   dvsfc_bl(i) = (-1./g*rcs) * dvsfc_bl(i)
   dusfc_ss(i) = (-1./g*rcs) * dusfc_ss(i)
   dvsfc_ss(i) = (-1./g*rcs) * dvsfc_ss(i)
   dusfc_fd(i) = (-1./g*rcs) * dusfc_fd(i)
   dvsfc_fd(i) = (-1./g*rcs) * dvsfc_fd(i)
enddo


   return                                                            
   end subroutine gwdo2d

end module module_bl_gwdo_gsl
