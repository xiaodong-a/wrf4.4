


MODULE module_cu_gf_wrfdrv
use module_gfs_physcons, g => con_g,                           &
                         cp => con_cp,                         &
                         xlv => con_hvap,                      &
                         r_v => con_rv
use module_cu_gf_deep, only: cup_gf,neg_check,autoconv,aeroevap
use module_cu_gf_sh, only: cup_gf_sh
use module_cu_gf_ctrans, only: neg_check_chem





















CONTAINS


   SUBROUTINE GFDRV(spp_conv,pattern_spp_conv,field_conv,       &
               DT,DX                                            &
              ,rho,RAINCV,PRATEC                                &
              ,U,V,t,W,q,p,pi                                   &
              ,dz8w,p8w                                         &
              ,htop,hbot,ktop_deep                              &
              ,HT,hfx,qfx,XLAND                                 &
              ,GDC,GDC2 ,kpbl,k22_shallow,kbcon_shallow         &
              ,ktop_shallow,xmb_shallow                         &
              ,ichoice,ishallow_g3                              &
              ,ids,ide, jds,jde, kds,kde                        &
              ,ims,ime, jms,jme, kms,kme                        &
              ,its,ite, jts,jte, kts,kte                        &
              ,periodic_x,periodic_y                            &
              ,RQVCUTEN,RQCCUTEN,RQICUTEN                       &
              ,RQVFTEN,RTHFTEN,RTHCUTEN,RTHRATEN                &
              ,rqvblten,rthblten                                &
              ,dudt_phy,dvdt_phy                                &
              ,chem,tracer,numgas                               &
              ,num_chem,chemopt,num_tracer,traceropt            &
              ,conv_tr_wetscav,conv_tr_aqchem,chem_conv_tr      &
                                                                )

   IMPLICIT NONE
      integer, parameter :: ideep=1
      integer, parameter :: imid_gf=0
      integer, parameter :: ichoicem=0  
      integer, parameter :: ichoice_s=0 
      integer, parameter :: dicycle=1 
      integer, parameter :: dicycle_m=0 
      real, parameter :: aodccn=0.1

   INTEGER,      INTENT(IN   ) ::                               &
                                  ids,ide, jds,jde, kds,kde,    & 
                                  ims,ime, jms,jme, kms,kme,    & 
                                  its,ite, jts,jte, kts,kte
   LOGICAL periodic_x,periodic_y
   integer, intent (in   )              :: ichoice
  
   INTEGER,      INTENT(IN   ) :: ishallow_g3

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                          U,    &
                                                          V,    &
                                                          W,    &
                                                         pi,    &
                                                          t,    &
                                                          q,    &
                                                          p,    &
                                                       dz8w,    &
                                                       p8w,    &
                                                        rho
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          OPTIONAL                                         ,    &
          INTENT(INOUT   ) ::                                   &
               GDC,GDC2

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(IN) :: hfx,qfx,HT,XLAND
   INTEGER, DIMENSION( ims:ime , jms:jme ),INTENT(IN) :: KPBL
   INTEGER, DIMENSION( ims:ime , jms:jme ),                     &
            OPTIONAL                      ,                     &
            INTENT(INOUT) :: k22_shallow,kbcon_shallow,ktop_shallow
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT  ),         &
            OPTIONAL  :: xmb_shallow

   REAL, INTENT(IN   ) :: DT, DX


   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(INOUT) ::           pratec,RAINCV,htop,hbot






   INTEGER, DIMENSION( ims:ime,         jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(  OUT) ::                           ktop_deep

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                           RTHFTEN,    &
                                                    RQVFTEN

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   RTHCUTEN,    &
                                                   RQVCUTEN,    &
                                                   RQVBLTEN,    &
                                                   RTHBLTEN,    &
                                                   RTHRATEN,    &
                                                   RQCCUTEN,    &
                                                   RQICUTEN
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                          DUDT_PHY,    &
                                                   DVDT_PHY

   INTEGER,INTENT(IN   ) ::                                     &
                                 numgas,chemopt,traceropt,      &
                                 num_tracer,num_chem,           &
                                 conv_tr_wetscav,conv_tr_aqchem,&
                                 chem_conv_tr
   REAL,DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ),     &
         INTENT(INOUT) ::                                       &
                                   chem
   REAL,DIMENSION( ims:ime , kms:kme , jms:jme, num_tracer ),   &
         INTENT(INOUT) ::                                       &
                                   tracer

   REAL,DIMENSION( its:ite , kts:kte , num_chem )::             &
                        chem2d,outchemts,outchemtm,             &
                        outchemt,totchemt
   REAL,DIMENSION( its:ite , kts:kte , num_tracer )::           &
                        tracer2d,outtracerts,outtracertm,       &
                        outtracert,tottracert
   INTEGER :: nv,iopt
   REAL:: epsilc


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),OPTIONAL  ::pattern_spp_conv,field_conv
   REAL, DIMENSION( its:ite, 4 )                 ::   rstochcol 

   REAL,  DIMENSION( its:ite )   :: rand_mom,rand_vmas
   REAL,  DIMENSION( its:ite,4 ) :: rand_clos









   INTEGER                                 :: spp_conv


     real,    dimension (its:ite,kts:kte) ::                    &
        dhdt
     real,    dimension (its:ite,kts:kte) ::                    &
        OUTT,OUTQ,OUTQC,cupclw,outu,outv,cnvwt
     real,    dimension (its:ite,kts:kte) ::                    &
        OUTTs,OUTQs,OUTQCs,cupclws,outus,outvs,cnvwts
     real,    dimension (its:ite,kts:kte) ::                    &
        OUTTm,OUTQm,OUTQCm,cupclwm,outum,outvm,cnvwtm
     real,    dimension (its:ite)         ::                    &
        pret, prets,pretm,ter11, aa0, xlandi
     real,    dimension (its:ite)         ::                    &
        hfxi,qfxi,dxi

     integer, dimension (its:ite) ::                            &
        ierr,ierrs,ierrm
     integer, dimension (its:ite) ::                            &
        kbcon, kbcons, kbconm,                                  &
        ktop, ktops, ktopm,                                     &
        kpbli, k22, k22s, k22m

     integer :: ibegc,iendc,jbegc,jendc

     integer, dimension (its:ite)         :: jmin,jminm





     real,    dimension (its:ite,kts:kte) ::                    &
        zo,T2d,q2d,PO,P2d,US,VS,rhoi,tn,qo,tshall,qshall

     real,    dimension (its:ite,kts:kte) ::                    &
        zus,zum,zu,zdm,zd
     real,    dimension (its:ite,kts:kte) ::                    &
        omeg
     real, dimension (its:ite)            ::                    &
        ccn,Z1,PSUR,cuten,cutens,cutenm,                        &
        umean,vmean,pmean,xmb,xmbs,                             &
        xmbm,xmb_out,tau_ecmwf_out,xmb_dumm
     real, dimension (its:ite)     ::                    &
        edt,edtm,mconv

   INTEGER :: i,j,k,ICLDCK,ipr,jpr,n
   REAL    :: tcrit,dp,dq
   INTEGER :: itf,jtf,ktf,iss,jss,nbegin,nend
   REAL    :: rkbcon,rktop        
   character*50 :: ierrc(its:ite)
   character*50 :: ierrcs(its:ite)
   character*50 :: ierrcm(its:ite)

     real,    dimension (its:ite,kts:kte) :: hco,hcdo,zdo
     real,    dimension (its:ite,10)         :: forcing,forcing2

     integer, dimension (its:ite) :: cactiv
     real,    dimension (its:ite,kts:kte) ::  qcheck

   epsilc=1.e-30
   iopt=0
   tcrit=258.
   ipr=0 
   jpr=0 
   rand_mom(:)    = 0.
   rand_vmas(:)   = 0.
   rand_clos(:,:) = 0.

   IF ( periodic_x ) THEN
      ibegc=max(its,ids)
      iendc=min(ite,ide-1)
   ELSE
      ibegc=max(its,ids+4)
      iendc=min(ite,ide-5)
   END IF
   IF ( periodic_y ) THEN
      jbegc=max(jts,jds)
      jendc=min(jte,jde-1)
   ELSE
      jbegc=max(jts,jds+4)
      jendc=min(jte,jde-5)
   END IF
   IF(PRESENT(k22_shallow)) THEN
   do j=jts,jte
   do i=its,ite
     k22_shallow(i,j)=0
     kbcon_shallow(i,j)=0
     ktop_shallow(i,j)=0
     xmb_shallow(i,j)=0
   enddo
   enddo
   endif
   rstochcol=0.0
   itf=MIN(ite,ide-1)
   ktf=MIN(kte,kde-1)
   jtf=MIN(jte,jde-1)

     DO J = jts,jte
     DO I= its,ite
     do k=kts,kte
       rthcuten(i,k,j)=0.
       rqvcuten(i,k,j)=0.
       IF(PRESENT(RQCCUTEN))rqccuten(i,k,j)=0.
       IF(PRESENT(RQICUTEN))rqicuten(i,k,j)=0.
       DUDT_PHY(I,K,J)=0.
       DVDT_PHY(I,K,J)=0.
     enddo
     enddo
     enddo

     DO 100 J = jts,jtf  

     DO I= its,itf

        if (spp_conv==1) then
        do n=1,4
        rstochcol(i,n)= pattern_spp_conv(i,n,j)
        if (pattern_spp_conv(i,n,j) .le. -1.0) then
          rstochcol(i,n)= -1.0
        endif
        if (pattern_spp_conv(i,n,j) .ge.  1.0) then
          rstochcol(i,n)=  1.0
        endif
        enddo
        endif
        ierrc(i)=" "
        ierrcs(i)=" "
        ierrcm(i)=" "
        ierr(i)=0
        ierrs(i)=0
        ierrm(i)=0

        cuten(i)=0.
        cutenm(i)=0.
        cutens(i)=1.
        if(ishallow_g3.eq.0)cutens(i)=0.

        kbcon(i)=0
        kbcons(i)=0
        kbconm(i)=0
        ktop(i)=0
        ktops(i)=0
        ktopm(i)=0
        xmb(i)=0.
        xmbs(i)=0.
        xmbm(i)=0.
        xmb_out(i)=0.
        xmb_dumm(i)=0.

        k22(i)=0
        k22s(i)=0
        k22m(i)=0

        HBOT(I,J)  =REAL(KTE)
        HTOP(I,J)  =REAL(KTS)
        raincv(i,j)=0.
        pratec (i,j)=0.
        xlandi(i)=xland(i,j)
        hfxi(i)=hfx(i,j)
        qfxi(i)=qfx(i,j)

        cactiv(i) = 0
        jmin(i) = 0
        jminm(i) = 0
        forcing(i,:)=0.
        forcing2(i,:)=0.
        tau_ecmwf_out(i) = 0.

        pret(i)=0.
        prets(i) = 0.
        pretm(i) = 0.

        mconv(i)=0.
        ccn(i)=150.

     ENDDO
     DO I= its,itf
        mconv(i)=0.
     ENDDO
     do k=kts,kte
     DO I= its,itf
         omeg(i,k)=0.
     ENDDO
     ENDDO



     DO I=ITS,ITF
         dxi(i)=dx
         PSUR(I)=p8w(I,1,J)*.01

         TER11(I)=max(0.,HT(i,j))

         hfxi(i)=hfx(i,j)
         qfxi(i)=qfx(i,j)
         pret(i)=0.
         umean(i)=0.
         vmean(i)=0.
         pmean(i)=0.
         kpbli(i)=kpbl(i,j)
         zo(i,kts)=ter11(i)+.5*dz8w(i,1,j)
         DO K=kts+1,ktf
         zo(i,k)=zo(i,k-1)+.5*(dz8w(i,k-1,j)+dz8w(i,k,j))
         enddo
     ENDDO

     DO K=kts,ktf
     DO I=ITS,ITF
         po(i,k)=p(i,k,j)*.01
         P2d(I,K)=PO(i,k)
         rhoi(i,k)=rho(i,k,j)
         US(I,K) =u(i,k,j)
         VS(I,K) =v(i,k,j)
         T2d(I,K)=t(i,k,j)
         q2d(I,K)=q(i,k,j)
         IF(Q2d(I,K).LT.1.E-08)Q2d(I,K)=1.E-08
         TN(I,K)=t2d(i,k)+(RTHFTEN(i,k,j)+RTHRATEN(i,k,j)+RTHBLTEN(i,k,j)) &
                          *pi(i,k,j)*dt
         QO(I,K)=q2d(i,k)+(RQVFTEN(i,k,j)+RQVBLTEN(i,k,j))*dt
         TSHALL(I,K)=t2d(i,k)+RTHBLTEN(i,k,j)*pi(i,k,j)*dt
         DHDT(I,K)=cp*RTHBLTEN(i,k,j)*pi(i,k,j)+ XLV*RQVBLTEN(i,k,j)
         QSHALL(I,K)=q2d(i,k)+RQVBLTEN(i,k,j)*dt
         IF(TN(I,K).LT.200.)TN(I,K)=T2d(I,K)
         IF(QO(I,K).LT.1.E-08)QO(I,K)=1.E-08
         OUTT(I,K)=0.
         OUTu(I,K)=0.
         OUTv(I,K)=0.
         OUTQ(I,K)=0.
         OUTQC(I,K)=0.
         OUTTm(I,K)=0.
         OUTum(I,K)=0.
         OUTvm(I,K)=0.
         OUTQm(I,K)=0.
         OUTQCm(I,K)=0.
         OUTTs(I,K)=0.
         OUTus(I,K)=0.
         OUTvs(I,K)=0.
         OUTQs(I,K)=0.
         OUTQCs(I,K)=0.
         cupclws(i,k) = 0.
         cupclw(i,k) = 0.
         cupclwm(i,k) = 0.
         qcheck(i,k) = 0.
         do nv=2,num_chem
           outchemts(I,K,nv)=0.
           outchemtm(I,K,nv)=0.
           outchemt(I,K,nv)=0.
           chem2d(I,K,nv)=max(epsilc,chem(i,k,j,nv))
         enddo
         do nv=2,num_tracer
           outtracerts(I,K,nv)=0.
           outtracertm(I,K,nv)=0.
           outtracert(I,K,nv)=0.
           tracer2d(I,K,nv)=max(epsilc,tracer(i,k,j,nv))
         enddo
     ENDDO
     ENDDO


     DO K=kts,ktf
     DO I=ITS,ITF
         omeg(I,K)= -g*rho(i,k,j)*w(i,k,j)
     enddo
     enddo
     do k=  kts+1,ktf-1
     DO I = its,itf
         if((p2d(i,1)-p2d(i,k)).gt.150.and.p2d(i,k).gt.300)then
            dp=-.5*(p2d(i,k+1)-p2d(i,k-1))
            umean(i)=umean(i)+us(i,k)*dp
            vmean(i)=vmean(i)+vs(i,k)*dp
            pmean(i)=pmean(i)+dp
         endif
     enddo
     enddo
      DO K=kts,ktf-1
      DO I = its,itf
        dq=(q2d(i,k+1)-q2d(i,k))
        mconv(i)=mconv(i)+omeg(i,k)*dq/g
      enddo
      ENDDO
      DO I = its,itf
        if(mconv(i).lt.0.)mconv(i)=0.
      ENDDO




       if(ishallow_g3 == 1 )then

          call CUP_gf_sh (                                              &

              zo,t2d,q2d,ter11,tshall,qshall,p2d,psur,dhdt,kpbli,      &
              rhoi,hfxi,qfxi,xlandi,ichoice_s,tcrit,dt,                  &


              zus,xmbs,kbcons,ktops,k22s,ierrs,ierrcs,    &

              outts,outqs,outqcs,cnvwt,prets,cupclws,             &
              num_chem,chem2d,outchemts,          &
              num_tracer,tracer2d,outtracerts,    &
              numgas,chemopt,traceropt,           &
              conv_tr_wetscav,conv_tr_aqchem,     &
              chem_conv_tr,                       &

              itf,ktf,its,ite, kts,kte,ipr)
          do i=its,itf
           if(xmbs(i).le.0.)cutens(i)=0.
          enddo
          CALL neg_check('shallow',ipr,dt,q2d,outqs,outts,outus,outvs,   &
                                 outqcs,prets,its,ite,kts,kte,itf,ktf)

        endif


   if(imid_gf == 1)then

      call cup_gf(        &
               itf,ktf,its,ite, kts,kte  &

              ,dicycle_m       &
              ,ichoicem       &
              ,ipr           &
              ,ccn           &
              ,dt         &
              ,imid_gf          &

              ,kpbli         &
              ,dhdt          &
              ,xlandi        &

              ,zo            &
              ,forcing2      &
              ,t2d           &
              ,q2d           &
              ,ter11         &
              ,tshall        &
              ,qshall        &
              ,p2d          &
              ,psur          &
              ,us            &
              ,vs            &
              ,rhoi          &
              ,hfxi          &
              ,qfxi          &
              ,dxi            &
              ,mconv         &
              ,omeg          &

              ,cactiv        &
              ,cnvwtm        &
              ,zum           &
              ,zdm           &
              ,edtm          &
              ,xmbm          &
              ,xmb_dumm      &
              ,xmbs          &
              ,pretm         &
              ,outum         &
              ,outvm         &
              ,outtm         &
              ,outqm         &
              ,outqcm        &
              ,kbconm        &
              ,ktopm         &
              ,cupclwm       &
              ,ierrm         &
              ,ierrcm        &

              ,rand_mom      & 
              ,rand_vmas     & 
              ,rand_clos     & 
              ,0             & 
                               
                               
                               
                               
                               
                               
              ,num_chem,chem2d,outchemtm       &
              ,num_tracer,tracer2d,outtracertm &
              ,numgas,chemopt,traceropt        &
              ,conv_tr_wetscav,conv_tr_aqchem  &
              ,chem_conv_tr                    &
              ,k22m          &
              ,jminm)

            DO I=its,itf
            DO K=kts,ktf
              qcheck(i,k)=q2d(i,k) +outqs(i,k)*dt
            enddo
            enddo
      CALL neg_check('mid',ipr,dt,qcheck,outqm,outtm,outum,outvm,   &
                     outqcm,pretm,its,ite,kts,kte,itf,ktf)
    endif

   if(ideep.eq.1)then
      call cup_gf(        &
               itf,ktf,its,ite, kts,kte  &

              ,dicycle       &
              ,ichoice       &
              ,ipr           &
              ,ccn           &
              ,dt            &
              ,0             &

              ,kpbli         &
              ,dhdt          &
              ,xlandi        &

              ,zo            &
              ,forcing       &
              ,t2d           &
              ,q2d           &
              ,ter11         &
              ,tn            &
              ,qo            &
              ,p2d           &
              ,psur          &
              ,us            &
              ,vs            &
              ,rhoi          &
              ,hfxi          &
              ,qfxi          &
              ,dxi            &
              ,mconv         &
              ,omeg          &

              ,cactiv       &
              ,cnvwt        &
              ,zu           &
              ,zd           &
              ,edt          &
              ,xmb          &
              ,xmbm         &
              ,xmbs         &
              ,pret         &
              ,outu         &
              ,outv         &
              ,outt         &
              ,outq         &
              ,outqc        &
              ,kbcon        &
              ,ktop         &
              ,cupclw       &
              ,ierr         &
              ,ierrc        &

              ,rand_mom      & 
              ,rand_vmas     & 
              ,rand_clos     & 
              ,0             & 
                               
                               
                               
                               
                               
                               
              ,num_chem,chem2d,outchemt        &
              ,num_tracer,tracer2d,outtracert  &
              ,numgas,chemopt,traceropt        &
              ,conv_tr_wetscav,conv_tr_aqchem  &
              ,chem_conv_tr                    & 
              ,k22          &
              ,jmin)
        jpr=0
        ipr=0
            DO I=its,itf
            DO K=kts,ktf
              qcheck(i,k)=q2d(i,k) +(outqs(i,k)+outqm(i,k))*dt
            enddo
            enddo
      CALL neg_check('deep',ipr,dt,qcheck,outq,outt,outu,outv,   &
                                         outqc,pret,its,ite,kts,kte,itf,ktf)

      endif
            if(j.lt.jbegc.or.j.gt.jendc)go to 100
        IF(PRESENT(k22_shallow)) THEN
             if(ishallow_g3.eq.1)then
               DO I=ibegc,iendc
                 xmb_shallow(i,j)=xmbs(i)
                 k22_shallow(i,j)=k22s(i)
                 kbcon_shallow(i,j)=kbcons(i)
                 ktop_shallow(i,j)=ktops(i)
                 ktop_deep(i,j) = ktop(i)
               ENDDO
            endif
         ENDIF
            DO I=ibegc,iendc
              cuten(i)=0.
              ktop_deep(i,j) = ktop(i)
              if(pret(i).gt.0.)then
                 cuten(i)=1.
              else
                 cuten(i)=0.
                 kbcon(i)=0
                 ktop(i)=0
              endif
              if(pretm(i).gt.0.)then
                 cutenm(i)=1.
              else
                 cutenm(i)=0.
                 kbconm(i)=0
                 ktopm(i)=0
              endif

            ENDDO
            DO I=ibegc,iendc
            DO K=kts,ktf
               RTHCUTEN(I,K,J)= (cutens(i)*outts(i,k)+ &
                                 cutenm(i)*outtm(i,k)+ &
                                 cuten(i)* outt(i,k)  )/pi(i,k,j)
               RQVCUTEN(I,K,J)= cuten(i)*outq(i,k)   + &
                                cutens(i)*outqs(i,k)+  &
                                cutenm(i)*outqm(i,k)
               DUDT_PHY(I,K,J)=outum(i,k)*cutenm(i)+outu(i,k)*cuten(i)
               DVDT_PHY(I,K,J)=outvm(i,k)*cutenm(i)+outv(i,k)*cuten(i)
            ENDDO
            ENDDO
            DO I=ibegc,iendc
            DO K=kts,ktf
               if ((chemopt>0) .and. (chem_conv_tr>0)) then
               do nv=2,num_chem
                 totchemt(i,k,nv)=outchemts(i,k,nv)*cutens(i)+ &
                                  outchemtm(i,k,nv)*cutenm(i)+ &
                                  outchemt(i,k,nv)*cuten(i)
               enddo 
               endif
               if ((traceropt>0) .and. (chem_conv_tr>0)) then
               do nv=2,num_tracer
                 tottracert(I,K,nv)=outtracerts(i,k,nv)*cutens(i)+ &
                                    outtracertm(i,k,nv)*cutenm(i)+ &
                                    outtracert(i,k,nv)*cuten(i)
               enddo
               endif
            ENDDO
            ENDDO

            if ((chemopt>0) .and. (chem_conv_tr>0)) then
              call neg_check_chem(ktop,dt,chem2d,totchemt,iopt,num_chem,    &
                                   its,ite,kts,kte,itf)
              DO I=ibegc,iendc
              DO K=kts,ktf
              do nv=2,num_chem
                chem(I,K,J,nv)=max(epsilc,chem(i,k,j,nv)+totchemt(i,k,nv)*dt)
              enddo
              ENDDO
              ENDDO
            endif
            if ((traceropt>0) .and. (chem_conv_tr>0)) then
               call neg_check_chem(ktop,dt,tracer2d,tottracert,iopt,num_chem, &
                                   its,ite,kts,kte,itf)
              DO I=ibegc,iendc
              DO K=kts,ktf
              do nv=2,num_tracer
                 tracer(I,K,J,nv)=max(epsilc,tracer(i,k,j,nv)+tottracert(i,k,nv)*dt)
              enddo 
              ENDDO
              ENDDO
            endif 

            DO I=ibegc,iendc
              if(pret(i).gt.0. .or. pretm(i).gt.0. .or. prets(i).gt.0.)then
                 pratec(i,j)=cuten(i)*pret(i)+cutenm(i)*pretm(i)+cutens(i)*prets(i)
                 raincv(i,j)=pratec(i,j)*dt
                 rkbcon = kte+kts - kbcon(i)
                 rktop  = kte+kts -  ktop(i)
                 if (ktop(i)  > HTOP(i,j)) HTOP(i,j) = ktop(i)+.001
                 if (kbcon(i) < HBOT(i,j)) HBOT(i,j) = kbcon(i)+.001
              endif
            ENDDO

            IF(PRESENT(RQCCUTEN)) THEN
                DO K=kts,ktf
                DO I=ibegc,iendc
                   RQCCUTEN(I,K,J)=outqcm(i,k)+outqcs(i,k)+outqc(I,K)*cuten(i)
                   IF ( PRESENT( GDC ) ) GDC(I,K,J)=cupclwm(i,k)+cupclws(i,k)+CUPCLW(I,K)*cuten(i)
                   IF ( PRESENT( GDC2 ) ) GDC2(I,K,J)=0.
                ENDDO
                ENDDO
            ENDIF

            IF(PRESENT(RQICUTEN).AND.PRESENT(RQCCUTEN))THEN
                DO K=kts,ktf
                  DO I=ibegc,iendc
                   if(t2d(i,k).lt.258.)then
                      RQICUTEN(I,K,J)=outqcm(i,k)+outqcs(i,k)+outqc(I,K)*cuten(i)
                      RQCCUTEN(I,K,J)=0.
                      IF ( PRESENT( GDC2 ) ) THEN
                        GDC2(I,K,J)=cupclwm(i,k)+cupclws(i,k)+CUPCLW(I,K)*cuten(i)
                        GDC(I,K,J) = 0.
                      ENDIF
                   else
                      RQICUTEN(I,K,J)=0.
                      RQCCUTEN(I,K,J)=outqcm(i,k)+outqcs(i,k)+outqc(I,K)*cuten(i)
                      IF ( PRESENT( GDC ) ) THEN
                        GDC(I,K,J)=cupclwm(i,k)+cupclws(i,k)+CUPCLW(I,K)*cuten(i)
                        GDC2(I,K,J) = 0.
                      ENDIF
                   endif
                ENDDO
                ENDDO
            ENDIF
 100    continue

   END SUBROUTINE GFDRV
END MODULE MODULE_CU_GF_WRFDRV
