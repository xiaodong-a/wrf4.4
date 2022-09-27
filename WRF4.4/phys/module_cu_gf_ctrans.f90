MODULE module_cu_gf_ctrans
  real, parameter::g=9.81
  INTEGER, allocatable :: HLC_ndx(:)

  contains

  SUBROUTINE ctrans_gf(numgas,num_chem,tracer,chemopt,traceropt  &
                      ,tracert,conv_tr_wetscav,conv_tr_aqchem &
                      ,po,po_cup,zo_cup &
                      ,zuo,zdo,pwo,pwdo,pwevo,pwavo &
                      ,up_massentro,up_massdetro &
                      ,dd_massentro,dd_massdetro &
                      ,tempco,clw_all  &
                      ,ktop,k22,kbcon,jmin  &
                      ,xmb,ierr,edto  &
                      ,itf,ktf,its,ite,kts,kte  &
                      ,ishallow)
  
  IMPLICIT NONE
     integer,intent (in   )                   ::            &
        itf,ktf,its,ite, kts,kte,                           &
        numgas,num_chem,conv_tr_wetscav,conv_tr_aqchem,     &
        chemopt,traceropt,ishallow
     integer,dimension (its:ite),intent (in  ) ::           &
        kbcon,ktop,k22,ierr,jmin
     real,   dimension (its:ite),intent (in  ) ::           &
        pwevo,pwavo,xmb,edto
     real,dimension (its:ite,kts:kte),intent (in )::        &
        po,po_cup,zo_cup,zuo,zdo,pwo,pwdo,tempco,clw_all,   &
        up_massentro,up_massdetro,dd_massentro,dd_massdetro
     REAL,DIMENSION(its:ite , kts:kte , num_chem),INTENT(IN)::     &
                        tracer
     REAL,DIMENSION(its:ite , kts:kte , num_chem),INTENT(INOUT)::  &
                        tracert

     INTEGER :: nv,i,k
     real, dimension(its:ite, kts:kte, num_chem) ::         &
               tr_up,tr_dd,tr_pw,tot_up_pw,tre_cup,tr_pwd
     real::dp


     call cup_env_clev_tr_gf(tracer,tre_cup,num_chem,ierr      &
                            ,itf,ktf,its,ite,kts,kte)


     call cup_up_tracer_gf(tracer,tre_cup,num_chem,numgas      &
                          ,chemopt,traceropt                   &
                          ,conv_tr_wetscav,conv_tr_aqchem      &
                          ,tr_up,tr_pw,tot_up_pw               &
                          ,zo_cup,po,po_cup,clw_all,tempco     &
                          ,zuo,up_massentro,up_massdetro       &
                          ,k22,kbcon,ktop,ierr                 &
                          ,itf,ktf,its,ite,kts,kte)

     if (ishallow==0) then
       call cup_dd_tracer_gf(num_chem,tracer,tre_cup           &
                            ,tot_up_pw                         &
                            ,tr_dd,tr_pwd,po_cup,pwdo          &
                            ,pwevo,pwavo,edto,zdo              &
                            ,dd_massentro,dd_massdetro         &
                            ,jmin,ierr                         &
                            ,itf,ktf,its,ite,kts,kte)
     endif

     do i=its,itf
     if (ierr(i)==0) then
     do k=kts,ktop(i)
       dp=100.*(po_cup(i,k)-po_cup(i,k+1))
       do nv=2,num_chem
         tracert(i,k,nv)=-(zuo(i,k+1)*(tr_up(i,k+1,nv)-tre_cup(i,k+1,nv)) -                 &
                           zuo(i,k  )*(tr_up(i,k  ,nv)-tre_cup(i,k  ,nv)))*g/dp*xmb(i)      &
                         +(zdo(i,k+1)*(tr_dd(i,k+1,nv)-tre_cup(i,k+1,nv)) -                 &
                           zdo(i,k  )*(tr_dd(i,k  ,nv)-tre_cup(i,k  ,nv)))*g/dp*edto(i)*xmb(i)
       enddo 
     enddo 
     endif 
     enddo 

  END SUBROUTINE ctrans_gf

  SUBROUTINE cup_env_clev_tr_gf(tracer,tre_cup,num_chem,ierr  &
                               ,itf,ktf,its,ite,kts,kte)
  IMPLICIT NONE
     integer,intent (in   )                   ::            &
        itf,ktf,its,ite, kts,kte,num_chem
     integer,dimension (its:ite),intent (in  ) ::           &
        ierr
     REAL,DIMENSION(its:ite, kts:kte, num_chem),INTENT(IN)::     &
                        tracer
     real,dimension(its:ite, kts:kte, num_chem),INTENT(INOUT) :: &
                        tre_cup

     integer::i,k,nv
     integer,parameter :: clev_opt=2  
     
     if (clev_opt == 1) then
     
       do i=its,itf
         if (ierr(i).eq.0) then 
           do nv=2,num_chem
             do k=kts+1,ktf
               tre_cup(i,k,nv)=0.5*(tracer(i,k-1,nv)+tracer(i,k,nv))
             enddo 
             tre_cup(i,kts,nv)=tracer(i,kts,nv)
           enddo 
         endif 
       enddo 
     else
     
       do i=its,itf
         if (ierr(i).eq.0) then
           do nv=2,num_chem
             do k=kts,ktf
               tre_cup(i,k,nv)=tracer(i,k,nv)
             enddo 
           enddo 
         endif 
       enddo 
     endif 

  END SUBROUTINE cup_env_clev_tr_gf
  
  SUBROUTINE cup_up_tracer_gf(tracer,tre_cup,num_chem,numgas      &
                             ,chemopt,traceropt                   &
                             ,conv_tr_wetscav,conv_tr_aqchem      &
                             ,tr_up,tr_pw,tot_up_pw               &
                             ,zo_cup,p,po_cup,clw_all,t           &
                             ,zuo,up_massentro,up_massdetro       &
                             ,k22,kbcon,ktop,ierr                 &
                             ,itf,ktf,its,ite,kts,kte)
  IMPLICIT NONE
     integer,intent (in   )                   ::            &
        itf,ktf,its,ite, kts,kte,                           &
        numgas,num_chem,conv_tr_wetscav,conv_tr_aqchem,     &
        chemopt,traceropt
     integer,dimension (its:ite),intent (in  ) ::           &
        kbcon,ktop,k22,ierr
     real,dimension (its:ite,kts:kte),intent (in )::        &
        p,po_cup,zo_cup,zuo,t,clw_all,                      &
        up_massentro,up_massdetro
     REAL,DIMENSION(its:ite , kts:kte , num_chem),INTENT(IN)::     &
                        tracer,tre_cup
     REAL,DIMENSION(its:ite , kts:kte , num_chem),INTENT(OUT)::  &
                        tr_up,tr_pw
     REAL,DIMENSION(its:ite , num_chem),INTENT(OUT)::       &
                        tot_up_pw

     INTEGER :: nv,i,k
     REAL,DIMENSION(its:ite,num_chem)::  &
                        tc_b
     REAL ::XZZ,XZD,XZE,denom,dz,dp

     do i=its,itf
       if (ierr(i)==0) then
         do k=kts,ktf
         do nv=2,num_chem
           tr_up(i,k,nv)=tre_cup(i,k,nv)
           tr_pw(i,k,nv)=0.
           tot_up_pw(i,nv)=0.
         enddo 
         enddo 
       endif 
     enddo 


     do i=its,itf
       if (ierr(i)==0) then
         do nv=2,num_chem
           call get_cloud_bc_chem(kte,tre_cup(i,1:kte,nv),tc_b(i,nv),k22(i))
           do k=kts,k22(i)
             tr_up(i,k,nv)=tc_b(i,nv)
           enddo 
         enddo 
       endif 
     enddo 


     DO i=its,itf
       if (ierr(i)==0) then
         do k=k22(i)+1,ktop(i)+1
           dz=zo_cup(i,k)-zo_cup(i,k-1)
           dp=100.*(po_cup(i,k)-po_cup(i,k+1))
          
           XZZ=zuo(i,k-1)
           XZD=0.5*up_massdetro(i,k-1)
           XZE=up_massentro(i,k-1)
           denom=(XZZ-XZD+XZE)
          
           do nv=2,num_chem
             if (denom>0) then
               tr_up(i,k,nv)=(tr_up(i,k-1,nv)*XZZ-tr_up(i,k-1,nv)*XZD &
                            +tracer(i,k-1,nv)*XZE)/denom
             else
               tr_up(i,k,nv)=tr_up(i,k-1,nv)
             endif 
           enddo 
          
           if ((conv_tr_aqchem==1).and.(chemopt>0)) then
             call aqchem_gf(chemopt,num_chem,p(i,k),t(i,k)     &
                           ,dz,tr_up(i,k,:),clw_all(i,k)       &
                           )
           endif
          
           if ((conv_tr_wetscav==1).and.(chemopt>0)) then
             do nv=2,num_chem
               call wetscav(tr_up(i,k,nv),tr_pw(i,k,nv) &
                           ,zuo(i,k),nv,p(i,k),t(i,k),clw_all(i,k),dz  &
                           ,chemopt,numgas,num_chem)
               tot_up_pw(i,nv)=tot_up_pw(i,nv)+tr_pw(i,k,nv)*dp/g
             enddo 
           endif 
         enddo 
       endif 
     ENDDO 
  END SUBROUTINE cup_up_tracer_gf
  
  SUBROUTINE cup_dd_tracer_gf(num_chem,tracer,tre_cup           &
                             ,tot_up_pw                         &
                             ,tr_dd,tr_pwd,po_cup,pwdo          &
                             ,pwevo,pwavo,edto,zdo              &
                             ,dd_massentro,dd_massdetro         &
                             ,jmin,ierr                         &
                             ,itf,ktf,its,ite,kts,kte)
    IMPLICIT NONE
     integer,intent (in   )                   ::            &
        itf,ktf,its,ite, kts,kte,                           &
        num_chem
     integer,dimension (its:ite),intent (in  ) ::           &
        ierr,jmin
     real,   dimension (its:ite),intent (in  ) ::           &
        pwevo,pwavo,edto
     real,dimension (its:ite,kts:kte),intent (in )::        &
        po_cup,zdo,pwdo,dd_massentro,dd_massdetro
     REAL,DIMENSION(its:ite , kts:kte , num_chem),INTENT(IN)::     &
                        tracer,tre_cup
     REAL,DIMENSION(its:ite, num_chem),INTENT(IN)::     &
                        tot_up_pw
     REAL,DIMENSION(its:ite , kts:kte , num_chem),INTENT(OUT)::  &
                        tr_pwd,tr_dd

     INTEGER :: nv,i,k
     real:: frac_evap,dp,XZZ,XZD,XZE,denom,pwdper
      do i=its,itf
       if (ierr(i)==0) then
         do k=kts,ktf
         do nv=2,num_chem
           tr_dd(i,k,nv)=0.
           tr_pwd(i,k,nv)=0.
         enddo 
         enddo 
       endif 
     enddo 

     do i=its,itf
       if (ierr(i)==0) then
         
         frac_evap = - pwevo(i)/(1.e-16+pwavo(i))
         

         
         k=jmin(i)
         pwdper = pwdo(i,k)/(1.e-16+pwevo(i)) *frac_evap  
         dp= 100.*(po_cup(i,k)-po_cup(i,k+1))
         do nv=2,num_chem
         
           tr_dd(i,k,nv)=tre_cup(i,k,nv)
           tr_pwd(i,k,nv)=-pwdper*tot_up_pw(i,nv)*g/dp
           tr_dd(i,k,nv)=tr_dd(i,k,nv)-tr_pwd(i,k,nv)
         enddo 
         
         
         do k=jmin(i)-1,kts,-1
           XZZ=              zdo(i,k+1)
           XZD= 0.5*dd_massdetro(i,k  )
           XZE=     dd_massentro(i,k  )
           denom =  (XZZ-XZD+XZE)
         
           do nv=2,num_chem
             if(denom > 0.) then
               tr_dd(i,k,nv) = (tr_dd(i,k+1,nv)*XZZ-tr_dd(i,k+1,nv)*XZD &
                               +tracer(i,k,nv)*XZE) / denom
             else
               tr_dd(i,k,nv) = tr_dd(i,k+1,nv)
             endif
           enddo
         
           dp= 100.*(po_cup(i,k)-po_cup(i,k+1))
         
           pwdper= pwdo(i,k)/(1.e-16+pwevo(i))
         
           pwdper= pwdper*frac_evap
         
           pwdper= min(1.,max(pwdper,0.))
           do nv=2,num_chem
         
             tr_pwd(i,k,nv)=-pwdper* tot_up_pw(i,nv)*g/dp 
         
         
             tr_dd(i,k,nv)= tr_dd(i,k,nv)-tr_pwd(i,k,nv) 
           enddo 
         enddo 
       endif 
     enddo 

  END SUBROUTINE cup_dd_tracer_gf


  SUBROUTINE wetscav(tr_up1d,tr_pw1d &
                    ,zu1d,nv,p1d,t1d,clw_all1d,dz &
                    ,chemopt,numgas,num_chem)
  USE module_HLawConst, only:HLC
  USE module_state_description, ONLY: mozart_mosaic_4bin_kpp, &
                              mozart_mosaic_4bin_aq_kpp, &
                              mozcart_kpp, t1_mozcart_kpp, &
                              p_nh3,p_h2o2,p_hno3,p_hcho,p_ch3ooh, &
                              p_c3h6ooh,p_paa,p_hno4,p_onit,p_mvk, &
                              p_macr,p_etooh,p_prooh,p_acetp,p_mgly, &
                              p_mvkooh,p_onitr,p_isooh,p_ch3oh,p_c2h5oh, &
                              p_glyald,p_hydrald,p_ald,p_isopn,p_alkooh, &
                              p_mekooh,p_tolooh,p_terpooh,p_nh3,p_xooh,  &
                              p_ch3cooh,p_so2,p_sulf,p_so4aj,p_nh4aj,    &
                              p_no3aj,p_bc1,p_oc1,p_dms,p_sulf,p_seas_1, &
                              p_seas_2,p_seas_3,p_seas_4,p_bc2,p_oc2, &
                              p_hcooh 
      IMPLICIT NONE
     integer,intent (in) ::         &
        chemopt,nv,numgas,num_chem
     real,intent (in )::            &
        p1d,t1d,clw_all1d,dz,zu1d
     REAL,INTENT(INOUT)::           &
        tr_up1d,tr_pw1d

     real::tr_c,trch,trcc,c0,dens,tfac, &
           aq_gas_ratio,kh,dk1s,dk2s,   &
           HLCnst1,HLCnst2,HLCnst3,     &
           HLCnst4,HLCnst5,HLCnst6,     &
           heff
     integer::HLndx
     real, parameter :: hion = 1.e-5
     real, parameter :: hion_inv = 1./hion
     real, parameter :: HL_t0 = 298.
     REAL, PARAMETER :: mwdry = 28.966  

     integer,parameter :: USE_ICE_RETENTION=1
     real::reteff

     tfac=(HL_t0-t1d)/(t1d*HL_t0)
     aq_gas_ratio=0.0
     dens=0.1*p1d/t1d*mwdry/8.314472  

     reteff = 0.
     if( nv == p_h2o2 ) then 
       reteff=.64
     elseif( nv == p_hno3 ) then
       reteff=1.
     elseif( nv == p_hcooh ) then
       reteff=.64
     elseif( nv == p_ch3ooh ) then
       reteff=.02
     elseif( nv == p_so2 ) then
       reteff= .02
     elseif( nv == p_hcooh ) then
       reteff= .68
     endif
     c0=0.004
     if (t1d < 273.15) c0=c0*exp(0.07*(t1d-273.15))


     if( chemopt == MOZCART_KPP .or. chemopt == T1_MOZCART_KPP .or. &
                chemopt == MOZART_MOSAIC_4BIN_KPP .or. &
                chemopt == MOZART_MOSAIC_4BIN_AQ_KPP ) then


       if ( .not. allocated(HLC_ndx) ) then
           call conv_tr_wetscav_init_phys( numgas, num_chem )
       endif
       HLndx = HLC_ndx(nv)

       if( HLndx /= 0 ) then
         HLCnst1 = HLC(HLndx)%hcnst(1) 
         HLCnst2 = HLC(HLndx)%hcnst(2)
         HLCnst3 = HLC(HLndx)%hcnst(3) 
         HLCnst4 = HLC(HLndx)%hcnst(4)
         HLCnst5 = HLC(HLndx)%hcnst(5) 
         HLCnst6 = HLC(HLndx)%hcnst(6)
         kh = HLCnst1 * exp( HLCnst2* tfac )
         if( HLCnst3 /= 0. ) then
           dk1s = HLCnst3 * exp( HLCnst4* tfac )
         else
           dk1s = 0.
         endif
         if( HLCnst5 /= 0. ) then
           dk2s = HLCnst5 * exp( HLCnst6* tfac )
         else
           dk2s = 0.
         endif
         if( nv /= p_nh3 ) then
           heff = kh*(1. + dk1s*hion_inv*(1. + dk2s*hion_inv))
         else
           heff = kh*(1. + dk1s*hion/dk2s)
         endif
         aq_gas_ratio = moz_aq_frac(t1d, clw_all1d*dens, heff)
       endif 
     else 
     
     
       if (nv .eq. p_h2o2)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 8.33e+04, 7379.)
       if (nv .eq. p_hno3)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 2.6e+06, 8700.)
       if (nv .eq. p_hcho)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 6.30e+03, 6425.)
       if (nv .eq. p_ch3ooh)  aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.11e+02, 5241.)
       if (nv .eq. p_c3h6ooh) aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 2.20e+02, 5653.)
       if (nv .eq. p_paa)     aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 8.37e+02, 5308.)
       if (nv .eq. p_hno4)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 1.2e+04, 6900.) 
       if (nv .eq. p_onit)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 1.00e+03, 6000.)
       if (nv .eq. p_mvk)     aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 1.7e-03, 0.)
       if (nv .eq. p_macr)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 1.70e-03, 0.)
       if (nv .eq. p_etooh)   aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.36e+02, 5995.)
       if (nv .eq. p_prooh)   aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.36e+02, 5995.)
       if (nv .eq. p_acetp)   aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.36e+02, 5995.)
       if (nv .eq. p_mgly)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.71e+03, 7541.)
       if (nv .eq. p_mvkooh)  aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 2.6e+06, 8700.)
       if (nv .eq. p_onitr)   aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 7.51e+03, 6485.)
       if (nv .eq. p_isooh)   aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 2.6e+06, 8700.)
       if (nv .eq. p_ch3oh)   aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 2.20e+02, 4934.)
       if (nv .eq. p_c2h5oh)  aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 2.00e+02, 6500.)
       if (nv .eq. p_glyald)  aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 4.14e+04, 4630.)
       if (nv .eq. p_hydrald) aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 7.00e+01, 6000.)
       if (nv .eq. p_ald)     aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 1.14e+01, 6267.)
       if (nv .eq. p_isopn)   aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 1.00e+01, 0.)
       if (nv .eq. p_alkooh)  aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.11e+02, 5241.)
       if (nv .eq. p_mekooh)  aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.11e+02, 5241.)
       if (nv .eq. p_tolooh)  aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.11e+02, 5241.)
       if (nv .eq. p_terpooh) aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 3.11e+02, 5241.)
       if (nv .eq. p_nh3)     aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 7.40e+01, 3400.)
       if (nv .eq. p_xooh)    aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 90.5, 5607.)
       if (nv .eq. p_ch3cooh) aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 4.1e3, 6300.)
       if (nv .eq. p_so2)     aq_gas_ratio = aq_frac(p1d*100., t1d, clw_all1d*dens, 1.2, 3100.)       
     endif 

     if ((USE_ICE_RETENTION==1).and.(t1d < 273.15)) aq_gas_ratio=reteff*aq_gas_ratio





     if (nv.gt.numgas) aq_gas_ratio = 0.5
     if (nv.eq.p_so4aj) aq_gas_ratio = 1.0
     if (nv.eq.p_nh4aj) aq_gas_ratio = 1.0
     if (nv.eq.p_no3aj) aq_gas_ratio = 1.0
     if (nv.eq.p_bc1 .or. nv.eq.p_oc1 .or. nv.eq.p_dms) aq_gas_ratio=0.
     if (nv.eq.p_sulf .or. nv.eq.p_seas_1 .or. nv.eq.p_seas_2) aq_gas_ratio=1.
     if (nv.eq.p_seas_3 .or. nv.eq.p_seas_4) aq_gas_ratio=1.
     if (nv.eq.p_bc2 .or. nv.eq.p_oc2) aq_gas_ratio=0.8

     if (aq_gas_ratio > 0.0) then
       tr_c = aq_gas_ratio*tr_up1d 
       trch = tr_up1d-tr_c 
       trcc = tr_c/(1.+c0*dz*zu1d) 
       tr_pw1d = c0*dz*trcc*zu1d 
       tr_up1d = trcc+trch 
     endif
  END SUBROUTINE wetscav

   SUBROUTINE conv_tr_wetscav_init_phys( numgas, num_chem )

   use module_state_description, only : param_first_scalar
   use module_scalar_tables, only : chem_dname_table
   use module_chem_utilities, only : UPCASE
   use module_HLawConst

   integer, intent(in) :: numgas, num_chem




   integer :: m, m1
   integer :: astat
   character(len=64) :: HL_tbl_name
   character(len=64) :: wrf_spc_name

is_allocated : &
   if( .not. allocated(HLC_ndx) ) then



     allocate( HLC_ndx(num_chem),stat=astat )
     if( astat /= 0 ) then
       call wrf_error_fatal3("<stdin>",488,&
"conv_tr_wetscav_init: failed to allocate HLC_ndx")
     endif
     HLC_ndx(:) = 0
     do m = param_first_scalar,numgas
       wrf_spc_name = chem_dname_table(1,m)
       call upcase( wrf_spc_name )
       do m1 = 1,nHLC
         HL_tbl_name = HLC(m1)%name
         call upcase( HL_tbl_name )
         if( trim(HL_tbl_name) == trim(wrf_spc_name) ) then
           HLC_ndx(m) = m1
           exit
         endif
       end do
     end do
   endif is_allocated

   END SUBROUTINE conv_tr_wetscav_init_phys




  REAL FUNCTION moz_aq_frac(T, q, heff )
   implicit none
    REAL, INTENT(IN)  :: T,           & 
                       q,           & 
                       heff      
    REAL, PARAMETER   :: Rgas = 8.31446 

    
    REAL              :: tr_air, tr_aq
    
    tr_air  = 1. / (Rgas * T)
    
    tr_aq   = heff * (q / 1000.0)
    moz_aq_frac = min( 1.0, max( 0.0, tr_aq / (tr_aq + tr_air) ) )
  END FUNCTION moz_aq_frac
  REAL FUNCTION aq_frac(p, T, q, Kh_298, dHoR)
    REAL, INTENT(IN)  :: p,           & 
                         T,           & 
                         q,           & 
                         Kh_298,      & 
                         dHoR           
    REAL, PARAMETER   :: Rgas = 8.31446 
    
    REAL              :: Kh, tr_air, tr_aq

    
    Kh      = Kh_298 * exp ( dHoR * ( 1.0/T - 1.0/298 ) ) * 101.325
    
    tr_air  = 1 / (Rgas * T)
    
    tr_aq   = Kh * (q / 1000.0)
    aq_frac = min( 1.0, max( 0.0, tr_aq / (tr_aq + tr_air) ) )
  END FUNCTION aq_frac
  SUBROUTINE aqchem_gf(chemopt,num_chem,p1d,t1d     &
                      ,dz,tr_up1d,clw_all1d          &
                      )
  USE module_ctrans_aqchem
  USE module_state_description, only:RADM2SORG,RADM2SORG_AQ,  &
                     RACMSORG_AQ,RACMSORG_KPP,RADM2SORG_KPP,  &
                         RACM_ESRLSORG_KPP,RACM_SOA_VBS_KPP,  &
                       RADM2SORG_AQCHEM,RACMSORG_AQCHEM_KPP,  &
                  CB05_SORG_VBS_AQ_KPP,RACM_SOA_VBS_HET_KPP,  &
           RACM_ESRLSORG_AQCHEM_KPP,RACM_SOA_VBS_AQCHEM_KPP,  &
           mozart_mosaic_4bin_kpp,mozart_mosaic_4bin_aq_kpp,  &
               p_so2,p_hno3,p_n2o5,p_nh3,p_h2o2,p_o3,p_sulf,  &
            p_facd,p_mepx,p_pacd,p_ora1,p_op1,p_paa,p_hcooh,  &
                    p_ch3ooh,p_sulf,p_so4aj,p_nh4aj,p_no3aj,  &
          p_so4_a01,p_so4_a02,p_so4_a03,p_so4_a04,p_nh4_a01,  &
          p_nh4_a02,p_nh4_a03,p_nh4_a04,p_no3_a01,p_no3_a02,  &
                                        p_no3_a03,p_no3_a04


  implicit none
   INTEGER,INTENT(IN) :: chemopt,num_chem
   real,intent(in) ::p1d,t1d,dz,clw_all1d
   real,dimension(num_chem), intent(inout) :: tr_up1d




      INTEGER, PARAMETER :: NGAS = 12  
      INTEGER, PARAMETER :: NAER = 36  
      INTEGER, PARAMETER :: NLIQS = 41 



      INTEGER, PARAMETER :: LSO2    =  1  
      INTEGER, PARAMETER :: LHNO3   =  2  
      INTEGER, PARAMETER :: LN2O5   =  3  
      INTEGER, PARAMETER :: LCO2    =  4  
      INTEGER, PARAMETER :: LNH3    =  5  
      INTEGER, PARAMETER :: LH2O2   =  6  
      INTEGER, PARAMETER :: LO3     =  7  
      INTEGER, PARAMETER :: LFOA    =  8  
      INTEGER, PARAMETER :: LMHP    =  9  
      INTEGER, PARAMETER :: LPAA    = 10  
      INTEGER, PARAMETER :: LH2SO4  = 11  
      INTEGER, PARAMETER :: LHCL    = 12  



      INTEGER, PARAMETER :: LSO4AKN  =  1  
      INTEGER, PARAMETER :: LSO4ACC  =  2  
      INTEGER, PARAMETER :: LSO4COR  =  3  
      INTEGER, PARAMETER :: LNH4AKN  =  4  
      INTEGER, PARAMETER :: LNH4ACC  =  5  
      INTEGER, PARAMETER :: LNO3AKN  =  6  
      INTEGER, PARAMETER :: LNO3ACC  =  7  
      INTEGER, PARAMETER :: LNO3COR  =  8  
      INTEGER, PARAMETER :: LORGAAKN =  9  
      INTEGER, PARAMETER :: LORGAACC = 10  
      INTEGER, PARAMETER :: LORGPAKN = 11  
      INTEGER, PARAMETER :: LORGPACC = 12  
      INTEGER, PARAMETER :: LORGBAKN = 13  
      INTEGER, PARAMETER :: LORGBACC = 14  
      INTEGER, PARAMETER :: LECAKN   = 15  
      INTEGER, PARAMETER :: LECACC   = 16  
      INTEGER, PARAMETER :: LPRIAKN  = 17  
      INTEGER, PARAMETER :: LPRIACC  = 18  
      INTEGER, PARAMETER :: LPRICOR  = 19  
      INTEGER, PARAMETER :: LNAAKN   = 20  
      INTEGER, PARAMETER :: LNAACC   = 21  
      INTEGER, PARAMETER :: LNACOR   = 22  
      INTEGER, PARAMETER :: LCLAKN   = 23  
      INTEGER, PARAMETER :: LCLACC   = 24  
      INTEGER, PARAMETER :: LCLCOR   = 25  
      INTEGER, PARAMETER :: LNUMAKN  = 26  
      INTEGER, PARAMETER :: LNUMACC  = 27  
      INTEGER, PARAMETER :: LNUMCOR  = 28  
      INTEGER, PARAMETER :: LSRFAKN  = 29  
      INTEGER, PARAMETER :: LSRFACC  = 30  
      INTEGER, PARAMETER :: LNACL    = 31  
      INTEGER, PARAMETER :: LCACO3   = 32  
      INTEGER, PARAMETER :: LMGCO3   = 33  
      INTEGER, PARAMETER :: LA3FE    = 34  
      INTEGER, PARAMETER :: LB2MN    = 35  
      INTEGER, PARAMETER :: LK       = 36  
      real,parameter::mwdry=28.966 
      REAL, PARAMETER :: mwso4 = 96.00   
      REAL, PARAMETER :: mwno3 = 62.0    
      REAL, PARAMETER :: mwnh4 = 18.0985 
      REAL, PARAMETER :: qcldwtr_cutoff = 1.0e-6 

     

     real precip,dens,airm,taucld 
     real, dimension (ngas) :: gas,gaswdep
     real, dimension (naer) :: aerosol,aerwdep
     real, dimension (nliqs) :: liquid
     real hpwdep
     real alfa0,alfa2,alfa3 
     real                                 ::                           &
       frac_so4(4), frac_no3(4), frac_nh4(4), tot_so4, tot_nh4, tot_no3

      
      
      

      if ((chemopt .EQ. RADM2SORG .OR. chemopt .EQ. RADM2SORG_AQ .OR. chemopt .EQ. RACMSORG_AQ .OR. &
           chemopt .EQ. RACMSORG_KPP .OR. chemopt .EQ. RADM2SORG_KPP .OR. chemopt .EQ. RACM_ESRLSORG_KPP .OR. &
           chemopt .EQ. RACM_SOA_VBS_KPP .OR. chemopt .EQ. RADM2SORG_AQCHEM .OR. chemopt .EQ. RACMSORG_AQCHEM_KPP .OR. &
           chemopt .EQ. CB05_SORG_VBS_AQ_KPP .OR.                                           &
           chemopt .EQ. RACM_SOA_VBS_HET_KPP .OR.   &
           chemopt .EQ. RACM_ESRLSORG_AQCHEM_KPP .OR. chemopt .EQ. RACM_SOA_VBS_AQCHEM_KPP) &
          ) then

        
        
        

        
        dens = 0.1*p1d/t1d*mwdry/8.314472 



        airm = 1000.0*dens*dz/mwdry 

        

        GASWDEP = 0.0
        AERWDEP = 0.0
        HPWDEP  = 0.0

        
        

        precip = 0.0 

        alfa0 = 0.0
        alfa2 = 0.0
        alfa3 = 0.0

        
        

        gas(:) = 0.0

        gas(lco2)   = 380.0e-6

        gas(lso2)   = tr_up1d(p_so2)*1.0e-6
        gas(lhno3)  = tr_up1d(p_hno3)*1.0e-6
        gas(ln2o5)  = tr_up1d(p_n2o5)*1.0e-6
        gas(lnh3)   = tr_up1d(p_nh3)*1.0e-6
        gas(lh2o2)  = tr_up1d(p_h2o2)*1.0e-6
        gas(lo3)    = tr_up1d(p_o3)*1.0e-6
        gas(lh2so4) = tr_up1d(p_sulf)*1.0e-6
        if (chemopt==CB05_SORG_VBS_AQ_KPP) then
           gas(lfoa)   = tr_up1d(p_facd)*1.0e-6
           gas(lmhp)   = tr_up1d(p_mepx)*1.0e-6
           gas(lpaa)   = tr_up1d(p_pacd)*1.0e-6
        else
           gas(lfoa)   = tr_up1d(p_ora1)*1.0e-6
           gas(lmhp)   = tr_up1d(p_op1)*1.0e-6
           gas(lpaa)   = tr_up1d(p_paa)*1.0e-6
        end if

        
        
        
        
        

        aerosol(:) = 0.0

        

        aerosol(lso4acc) = tr_up1d(p_so4aj)*1.0e-9*mwdry/mwso4
        aerosol(lnh4acc) = tr_up1d(p_nh4aj)*1.0e-9*mwdry/mwnh4
        aerosol(lno3acc) = tr_up1d(p_no3aj)*1.0e-9*mwdry/mwno3

        
        taucld = 1800.0

        if (clw_all1d*dens .gt. qcldwtr_cutoff) then 
          CALL AQCHEM( &
           t1d, &
           p1d*100., &
           taucld, &
           precip, &
           clw_all1d*dens, &
           clw_all1d*dens, &
           airm, &
           ALFA0, &
           ALFA2, &
           ALFA3, &
           GAS, &
           AEROSOL, &
           LIQUID, &
           GASWDEP, &
           AERWDEP, &
           HPWDEP)
        endif

        
        

        tr_up1d(p_so2)  =  gas(lso2)*1.0e6
        tr_up1d(p_hno3) =  gas(lhno3)*1.0e6
        tr_up1d(p_n2o5) =  gas(ln2o5)*1.0e6
        tr_up1d(p_nh3)  =  gas(lnh3)*1.0e6
        tr_up1d(p_h2o2) =  gas(lh2o2)*1.0e6
        tr_up1d(p_o3)   =  gas(lo3)*1.0e6
        tr_up1d(p_sulf) =  gas(lh2so4)*1.0e6
        if (chemopt==CB05_SORG_VBS_AQ_KPP) then
           tr_up1d(p_facd) =  gas(lfoa)*1.0e6
           tr_up1d(p_mepx)  =  gas(lmhp)*1.0e6
           tr_up1d(p_pacd)  =  gas(lpaa)*1.0e6
        else
           tr_up1d(p_ora1) =  gas(lfoa)*1.0e6
           tr_up1d(p_op1)  =  gas(lmhp)*1.0e6
           tr_up1d(p_paa)  =  gas(lpaa)*1.0e6
        end if

        
        

        tr_up1d(p_so4aj) = aerosol(lso4acc)*1.0e9*mwso4/mwdry
        tr_up1d(p_nh4aj) = aerosol(lnh4acc)*1.0e9*mwnh4/mwdry
        tr_up1d(p_no3aj) = aerosol(lno3acc)*1.0e9*mwno3/mwdry
      else if ((chemopt .EQ. mozart_mosaic_4bin_kpp .OR. &
                chemopt .EQ. mozart_mosaic_4bin_aq_kpp)  &
               ) then

        
        
        

        
        dens = 0.1*p1d/t1d*mwdry/8.314472 

        
        airm = 1000.0*dens*dz/mwdry 

        

        GASWDEP = 0.0
        AERWDEP = 0.0
        HPWDEP  = 0.0

        
        

        precip = 0.0 

        alfa0 = 0.0
        alfa2 = 0.0
        alfa3 = 0.0

        
        

        gas(:) = 0.0

        gas(lco2)   = 380.0e-6

        gas(lso2)   = tr_up1d(p_so2)*1.0e-6
        gas(lhno3)  = tr_up1d(p_hno3)*1.0e-6
        gas(ln2o5)  = tr_up1d(p_n2o5)*1.0e-6
        gas(lnh3)   = tr_up1d(p_nh3)*1.0e-6
        gas(lh2o2)  = tr_up1d(p_h2o2)*1.0e-6
        gas(lo3)    = tr_up1d(p_o3)*1.0e-6
        gas(lfoa)   = tr_up1d(p_hcooh)*1.0e-6
        gas(lmhp)   = tr_up1d(p_ch3ooh)*1.0e-6
        gas(lpaa)   = tr_up1d(p_paa)*1.0e-6
        gas(lh2so4) = tr_up1d(p_sulf)*1.0e-6

        
        
        
        
        

        aerosol(:) = 0.0

        

        
        
        frac_so4(:) = 0.25
        frac_nh4(:) = 0.25
        frac_no3(:) = 0.25

        tot_so4     = tr_up1d(p_so4_a01)+tr_up1d(p_so4_a02)+&
                      tr_up1d(p_so4_a03)+tr_up1d(p_so4_a04)
        tot_nh4     = tr_up1d(p_nh4_a01)+tr_up1d(p_nh4_a02)+&
                      tr_up1d(p_nh4_a03)+tr_up1d(p_nh4_a04)
        tot_no3     = tr_up1d(p_no3_a01)+tr_up1d(p_no3_a02)+&
                      tr_up1d(p_no3_a03)+tr_up1d(p_no3_a04)

        if (tot_so4 > 0.0) then
          frac_so4(1) = tr_up1d(p_so4_a01) / tot_so4
          frac_so4(2) = tr_up1d(p_so4_a02) / tot_so4
          frac_so4(3) = tr_up1d(p_so4_a03) / tot_so4
          frac_so4(4) = tr_up1d(p_so4_a04) / tot_so4
          aerosol(lso4acc) = tot_so4 *1.0e-9*mwdry/mwso4
        end if

        if (tot_nh4 > 0.0) then
          frac_nh4(1) = tr_up1d(p_nh4_a01) / tot_nh4
          frac_nh4(2) = tr_up1d(p_nh4_a02) / tot_nh4
          frac_nh4(3) = tr_up1d(p_nh4_a03) / tot_nh4
          frac_nh4(4) = tr_up1d(p_nh4_a04) / tot_nh4
          aerosol(lnh4acc) = tot_nh4 *1.0e-9*mwdry/mwnh4
        end if

        if (tot_no3 > 0.0) then
          frac_no3(1) = tr_up1d(p_no3_a01) / tot_no3
          frac_no3(2) = tr_up1d(p_no3_a02) / tot_no3
          frac_no3(3) = tr_up1d(p_no3_a03) / tot_no3
          frac_no3(4) = tr_up1d(p_no3_a04) / tot_no3
          aerosol(lno3acc) = tot_no3 *1.0e-9*mwdry/mwno3
        end if

        
        taucld = 1800.0

        if (clw_all1d*dens .gt. qcldwtr_cutoff) then 
          CALL AQCHEM( &
           t1d, &
           p1d*100., &
           taucld, &
           precip, &
           clw_all1d*dens, &
           clw_all1d*dens, &
           airm, &
           ALFA0, &
           ALFA2, &
           ALFA3, &
           GAS, &
           AEROSOL, &
           LIQUID, &
           GASWDEP, &
           AERWDEP, &
           HPWDEP)
      endif

        
        

        tr_up1d(p_so2)    =  gas(lso2)*1.0e6
        tr_up1d(p_hno3)   =  gas(lhno3)*1.0e6
        tr_up1d(p_n2o5)   =  gas(ln2o5)*1.0e6
        tr_up1d(p_nh3)    =  gas(lnh3)*1.0e6
        tr_up1d(p_h2o2)   =  gas(lh2o2)*1.0e6
        tr_up1d(p_o3)     =  gas(lo3)*1.0e6
        tr_up1d(p_hcooh)  =  gas(lfoa)*1.0e6
        tr_up1d(p_ch3ooh) =  gas(lmhp)*1.0e6
        tr_up1d(p_paa)    =  gas(lpaa)*1.0e6
        tr_up1d(p_sulf)   =  gas(lh2so4)*1.0e6

        
        

        tr_up1d(p_so4_a01) = aerosol(lso4acc) * frac_so4(1) * 1.0e9*mwso4/mwdry
        tr_up1d(p_so4_a02) = aerosol(lso4acc) * frac_so4(2) * 1.0e9*mwso4/mwdry
        tr_up1d(p_so4_a03) = aerosol(lso4acc) * frac_so4(3) * 1.0e9*mwso4/mwdry
        tr_up1d(p_so4_a04) = aerosol(lso4acc) * frac_so4(4) * 1.0e9*mwso4/mwdry

        tr_up1d(p_nh4_a01) = aerosol(lnh4acc) * frac_nh4(1) * 1.0e9*mwnh4/mwdry
        tr_up1d(p_nh4_a02) = aerosol(lnh4acc) * frac_nh4(2) * 1.0e9*mwnh4/mwdry
        tr_up1d(p_nh4_a03) = aerosol(lnh4acc) * frac_nh4(3) * 1.0e9*mwnh4/mwdry
        tr_up1d(p_nh4_a04) = aerosol(lnh4acc) * frac_nh4(4) * 1.0e9*mwnh4/mwdry

        tr_up1d(p_no3_a01) = aerosol(lno3acc) * frac_no3(1) * 1.0e9*mwno3/mwdry
        tr_up1d(p_no3_a02) = aerosol(lno3acc) * frac_no3(2) * 1.0e9*mwno3/mwdry
        tr_up1d(p_no3_a03) = aerosol(lno3acc) * frac_no3(3) * 1.0e9*mwno3/mwdry
        tr_up1d(p_no3_a04) = aerosol(lno3acc) * frac_no3(4) * 1.0e9*mwno3/mwdry
      endif
  END SUBROUTINE aqchem_gf

  SUBROUTINE neg_check_chem(ktop,dt,q,outq,iopt,num_chem,    &
                           its,ite,kts,kte,itf)
  implicit none
   INTEGER,INTENT(IN) :: iopt,num_chem,its,ite,kts,kte,itf

   real,dimension(its:ite,kts:kte,num_chem),                 &
        intent(inout) ::                                     &
                         outq
   real,dimension(its:ite,kts:kte,num_chem),                 &
        intent(in   ) ::                                     &
                         q
   integer,dimension(its:ite),                               &
        intent(in   ) ::                                     &
                         ktop
   real,intent(in   ) ::                                     &
                         dt
   real :: tracermin,tracermax,thresh,qmem,qmemf,qmem2,qtest,qmem1
   integer :: nv, i, k








     thresh=1.e-30
     if (iopt==0) then 
       do nv=2,num_chem
       do 100 i=its,itf
         tracermin=q(i,kts,nv)
         tracermax=q(i,kts,nv)
         do k=kts+1,kte-1
           tracermin=min(tracermin,q(i,k,nv))
           tracermax=max(tracermax,q(i,k,nv))
         enddo 
         tracermin=max(tracermin,thresh)
         qmemf=1.



         do k=kts,ktop(i)



           qmem=outq(i,k,nv)



           if(qmem.lt.0.)then
             qtest=q(i,k,nv)+outq(i,k,nv)*dt
             if(qtest.lt.tracermin)then



               qmem1=outq(i,k,nv)
               qmem2=(tracermin-q(i,k,nv))/dt
               qmemf=min(qmemf,qmem2/qmem1)
               if(qmemf.gt.1.)print *,'something wrong in negct_1',qmem2,qmem1
               qmemf=max(qmemf,0.)
             endif 
           endif 
         enddo 
         do k=kts,ktop(i)
            outq(i,k,nv)=outq(i,k,nv)*qmemf
         enddo 



         qmemf=1.
         do k=kts,ktop(i)



           qmem=outq(i,k,nv)



           if(qmem.gt.0.)then
             qtest=q(i,k,nv)+outq(i,k,nv)*dt
             if(qtest.gt.tracermax)then



               qmem1=outq(i,k,nv)
               qmem2=(tracermax-q(i,k,nv))/dt
               qmemf=min(qmemf,qmem2/qmem1)
               if(qmemf.gt.1.)print *,'something wrong in negct_2',qmem2,qmem1
               qmemf=max(qmemf,0.)
             endif 
           endif 
         enddo 
         do k=kts,ktop(i)
            outq(i,k,nv)=outq(i,k,nv)*qmemf
         enddo 
 100  continue 
      enddo 



    elseif(iopt.eq.1)then
      do i=its,itf
        qmemf=1.
        do k=kts,ktop(i)
        do nv=2,num_chem



          qmem=outq(i,k,nv)



          if(qmem.lt.0.)then
            qtest=q(i,k,nv)+outq(i,k,nv)*dt
            if(qtest.lt.thresh)then



              qmem1=outq(i,k,nv)
              qmem2=(thresh-q(i,k,nv))/dt
              qmemf=min(qmemf,qmem2/qmem1)
              qmemf=max(0.,qmemf)
            endif 
          endif 
        enddo 
        enddo 
        do nv=2,num_chem
        do k=kts,ktop(i)
          outq(i,k,nv)=outq(i,k,nv)*qmemf
        enddo 
        enddo 
      enddo 
    endif 

   END SUBROUTINE neg_check_chem
 SUBROUTINE get_cloud_bc_chem(mzp,array,x_aver,k22,add)
    implicit none
    integer, intent(in)     :: mzp,k22
    real   , intent(in)     :: array(mzp)
    real   , optional , intent(in)  :: add
    real   , intent(out)    :: x_aver
    integer :: i,local_order_aver,order_aver

    
    
    
    
    order_aver = 3 

    local_order_aver=min(k22,order_aver)

    x_aver=0.
    do i = 1,local_order_aver
      x_aver = x_aver + array(k22-i+1)
    enddo
      x_aver = x_aver/float(local_order_aver)
    if(present(add)) x_aver = x_aver + add

 end SUBROUTINE get_cloud_bc_chem

END MODULE module_cu_gf_ctrans
