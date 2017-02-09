!*******************************************************************
!**           S U B R O U T I N E   E N V I R P
!*******************************************************************
subroutine envirp

USE GLOBAL; USE MAIN;use NAMESC; use screenc, only:nit,jday;use tvdc, only: constituents; use rstart, only:eltm; use GEOMC, only:depthb
USE ENVIRPMOD
IMPLICIT NONE
save

!  initializing variables at first call
 if(NIT==1.or.iopenfish==0)then
      dltt=dlt
      allocate(cc_e(NCT),c_int(NCT),c_top(NCT),cd_e(NDC),cd_int(NDC),cd_top(NDC),c_avg(NCT),cd_avg(NDC),cn_e(NCT),cdn_e(NDC))
      cc_e='   '
      c_int=0.0
      c_top=0.0
      cd_e='   '
      cd_int=0.0
      cd_top=0.0
      c_avg=0.0
      cd_avg=0.0
      cn_e=0.0
      cdn_e=0.0
      NAC_E=0
      NACD_E=0
      CONE=NUNIT; NUNIT=NUNIT+1
      open(CONE,file='w2_envirprf.npt',status='old')
      READ (CONE,1200) numclass,selectivec,sjday1,sjday2,istart,iend
      Read (CONE,1201) VEL_VPR, VEL_INT, VEL_TOP,TEMP_VPR,TEMP_INT,TEMP_TOP, depth_vpr,d_int,d_top
      READ (CONE,1050) (CC_E(JC), C_INT(JC), C_TOP(JC), JC=1,NCT)
      READ (CONE,1050) (CD_E(JD),CD_INT(JD), CD_TOP(JD), JD=1,NDC)
      CLOSE(CONE)

          DO JC=1,NCT
          IF (CC_E(JC).EQ.' ON') THEN
            NAC_E     = NAC_E+1
            CN_E(NAC_E) = JC
          END IF
          End DO
          DO JD=1,NDC
          IF (CD_E(JD).EQ.' ON') THEN
            NACD_E     = NACD_E+1
            CDN_E(NACD_E) = JD
          END IF
          End DO

 1050 FORMAT(//(8X,(5X,A3,F8.0,F8.0)))
 1200 FORMAT(//8X,I8,5x,a3,f8.0,f8.0,i8,i8)
 1201 format(//8x,3(5x,a3,f8.3,f8.3))

    allocate (c_cnt(NCT),cd_cnt(NDC),c_class(NCT,numclass),cd_class(NDC,numclass),c_tot(NCT),cd_tot(NDC),t_class(numclass),v_class(numclass),c_sum(NCT),cd_sum(NDC))
    allocate (conc_c(NCT,numclass),conc_cd(NDC,numclass))
    allocate(d_class(numclass))
        cd_cnt=0.0
        c_cnt=0.0
        c_class=0.0
        cd_class=0.0
        c_tot=0.0
        cd_tot=0.0
        
        v_cnt=0.0
        v_class=0.0
        v_tot=0.0

        t_cnt=0.0
        t_class=0.0
        t_tot=0.0
        
        d_cnt=0.0
        d_class=0.0
        d_tot=0.0

      sumvolt=0.0
  else
      dltt=(jday-timlast)*86400.
  end if

  if(iopenfish.eq.3)go to 650     !iopenfish=3 is end of simulation deallocate arrays
  
  if(selectivec == ' ON')then
      if(jday < sjday1  .or. jday > sjday2)go to 650
  endif
  
   volgL=0.0
! start loop for succeeding calls to subroutine

do JW=1,NWB
  do jb=bs(JW),be(JW)
      do i=cus(jb),ds(jb)
        if(selectivec == ' ON')then
            if(i < istart .or. i > iend)then
                exit
            endif
        endif
          
! Depth
           if(depth_vpr.eq.' ON')then
            d_tot=d_tot+depthb(kb(i),i)*dltt
            d_cnt=d_cnt+dltt
            d_crit=d_top
            if(depthb(kb(i),i).ge.d_top)d_class(1)=d_class(1)+dltt
            do jj=2,numclass
              if(depthb(kb(i),i).lt.d_crit.and.depthb(kb(i),i).ge.d_crit-d_int)then
                d_class(jj)=d_class(jj)+dltt
                go to 300
              else
                d_crit=d_crit-d_int
              end if
              if(jj.eq.numclass.and.depthb(kb(i),i).lt.d_crit+d_int)then
                d_class(jj)=d_class(jj)+dltt
              end if
            end do
          end if
300       continue

          
          
          
          
        do k=KTWB(JW),kb(i)
            volgL=volgL+vol(K,I)

! Temperature 

        if(temp_vpr.eq.' ON')then

            t_tot=t_tot+t2(k,i)*VOL(k,i)*dltt
            t_cnt=t_cnt+vol(k,i)*dltt
            t_crit=temp_top
            if(t2(k,i).ge.temp_top)t_class(1)=t_class(1)+dltt*vol(k,i)
            do jj=2,numclass
              if(t2(k,i).lt.t_crit.and.t2(k,i).ge.t_crit-temp_int)then
                t_class(jj)=t_class(jj)+dltt*vol(k,i)
                go to 200
              else
                t_crit=t_crit-temp_int
              end if
              if(jj.eq.numclass.and.t2(k,i).lt.t_crit+temp_int)then
                t_class(jj)=t_class(jj)+dltt*vol(k,i)
              end if
            end do
          end if
200       continue


! Velocity

        if(vel_vpr.eq.' ON')then

            v_tot=v_tot+u(k,i)*vol(k,i)*dltt
            v_cnt=v_cnt+vol(k,i)*dltt
            v_crit=vel_top
            if(u(k,i).ge.vel_top)v_class(1)=v_class(1)+dltt*vol(k,i)
            do jj=2,numclass
              if(u(k,i).lt.v_crit.and.u(k,i).ge.v_crit-vel_int)then
                v_class(jj)=v_class(jj)+dltt*vol(k,i)
                go to 210
              else
                v_crit=v_crit-vel_int
              end if
              if(jj.eq.numclass.and.u(k,i).lt.v_crit+vel_int)then
                v_class(jj)=v_class(jj)+dltt*vol(k,i)
              end if
            end do
          end if
210       continue


! Constituents
  IF(CONSTITUENTS)THEN
            do jc=1,nac_e
              jac=cn_e(jc)

            c_tot(jc)=c_tot(jc)+c2(k,i,jac)*cmult(jac)*vol(k,i)*dltt
            c_cnt(jc)=c_cnt(jc)+vol(k,i)*dltt
            c_crit=c_top(jac)
            if(c2(k,i,jac)*cmult(jac).ge.c_top(jac))c_class(jc,1)=c_class(jc,1)+dltt*vol(k,i)
            do jj=2,numclass
              if(c2(k,i,jac)*cmult(jac).lt.c_crit.and.c2(k,i,jac)*cmult(jac).ge.c_crit-c_int(jac))then
                c_class(jc,jj)=c_class(jc,jj)+dltt*vol(k,i)
                go to 220
              else
                c_crit=c_crit-c_int(jac)
              end if
              if(jj.eq.numclass.and.c2(k,i,jac)*cmult(jac).lt.c_crit+c_int(jac))then
                c_class(jc,jj)=c_class(jc,jj)+dltt*vol(k,i)
              end if
            end do
          
220       continue
    

            end do


! Derived Constituents

            do jc=1,nacd_e

            jacd=cdn_e(jc)

            cd_tot(jc)=cd_tot(jc)+cd(k,i,jacd)*CDMULT(jacd)*vol(k,i)*dltt
            cd_cnt(jc)=cd_cnt(jc)+vol(k,i)*dltt
            cd_crit=cd_top(jacd)
            if(cd(k,i,jacd)*CDMULT(jacd).ge.cd_top(jacd))cd_class(jc,1)=cd_class(jc,1)+dltt*vol(k,i)
            do jj=2,numclass
              if(cd(k,i,jacd)*CDMULT(jacd).lt.cd_crit.and.cd(k,i,jacd)*CDMULT(jacd).ge.cd_crit-cd_int(jacd))then
                cd_class(jc,jj)=cd_class(jc,jj)+dltt*vol(k,i)
                go to 240
              else
                cd_crit=cd_crit-cd_int(jacd)
              end if
              if(jj.eq.numclass.and.cd(k,i,jacd)*CDMULT(jacd).lt.cd_crit+cd_int(jacd))then
                cd_class(jc,jj)=cd_class(jc,jj)+dltt*vol(k,i)
              end if
            end do
  240       continue

            end do
  ENDIF            
       end do
     end do
  end do
end do

! sum of volgL*dltt for volume fraction calculation

   sumvolt=sumvolt+VOLGL*dltt

650   continue

      if(iopenfish == 3)then
!  calculating average violation concentration and writing to file
        cd_sum=0.0
        c_sum=0.0
        T_SUM=0.0
        V_SUM=0.0
        
        if(temp_vpr.eq.' ON')then
          if(t_cnt.gt.0.0)then
          t_avg=t_tot/t_cnt
          else
          t_avg=0.0
          end if
        open(CONE,file='envrprf_t.dat',status='unknown')
        write(CONE,*)'"Temperature interval","Fraction of volume"'
        temp_c=temp_top
          do i=1,numclass
          write(CONE,125)temp_c,t_class(i)/sumvolt
          temp_c=temp_c-temp_int
          t_sum=t_sum+t_class(i)/sumvolt
          end do
        write(CONE,'(1x)')
        write(CONE,'(" 0 ",e12.4)')t_sum
        write(CONE,'(1x)')
        write(CONE,'(" 0 ",e12.4)')t_avg
        close(CONE)
        end if

        if(vel_vpr.eq.' ON')then
          if(v_cnt.gt.0.0)then
          v_avg=v_tot/v_cnt
          else
          v_avg=0.0
          end if
        open(CONE,file='envrprf_v.dat',status='unknown')
        write(CONE,*)'"Velocity interval","Fraction of volume"'
        vel_c=vel_top
          do i=1,numclass
          write(CONE,125)vel_c,v_class(i)/sumvolt
          vel_c=vel_c-vel_int
          v_sum=v_sum+v_class(i)/sumvolt
          end do
        write(CONE,'(1x)')
        write(CONE,'(" 0 ",e12.4)')v_sum
        write(CONE,'(1x)')
        write(CONE,'(" 0 ",e12.4)')v_avg
        close(CONE)
        end if
        
        
        if(depth_vpr.eq.' ON')then
          if(d_cnt.gt.0.0)then
          d_avg=d_tot/d_cnt
          else
          d_avg=0.0
          end if
        open(CONE,file='envrprf_depth.dat',status='unknown')
        write(CONE,*)'"Depth interval","Fraction of time"'
        d_c=d_top
          do i=1,numclass
          write(CONE,125)d_c,d_class(i)/d_cnt
          d_c=d_c-d_int
          d_sum=d_sum+d_class(i)/d_cnt
          end do
        write(CONE,'(1x)')
        write(CONE,'(" 0 ",e12.4)')d_sum
        write(CONE,'(1x)')
        write(CONE,'(" 0 ",e12.4)')d_avg
        close(CONE)
        end if
        
if(nac_e > 0)then
       open(CONE,file='envrprf_c.dat',status='unknown')
       write(CONE,4000)(cname2(cn_e(jc)),jc=1,nac_e)
4000 format(<nac_e>(' "',a8,'interval" "Fraction of volume" '))
       do jc=1,nac_e
        if(c_cnt(jc).gt.0.0)then
        c_avg(jc)=c_tot(jc)/c_cnt(jc)
        else
        c_avg(jc)=0.0
        end if
          do i=1,numclass
             if(i.eq.1)then
             conc_c(jc,i)=c_top(cn_e(jc))
             else
             conc_c(jc,i)=conc_c(jc,i-1)-c_int(cn_e(jc))
             end if
          c_sum(jc)=c_sum(jc)+c_class(jc,i)/sumvolt
          end do
       end do
        do i=1,numclass
        write(CONE,126)(conc_c(jc,i),c_class(jc,i)/sumvolt,jc=1,nac_e)
        end do
        write(CONE,'(1x)')
        write(CONE,129)(c_sum(jc),jc=1,nac_e)
        write(CONE,'(1x)')
        write(CONE,129)(c_avg(jc),jc=1,nac_e)
        close(CONE)
      
  if(nacd_e > 0)then     
       open(CONE,file='envrprf_cd.dat',status='unknown')
     write(CONE,4001)(cdname2(cdn_e(jc)),jc=1,nacd_e)
4001 format(<nacd_e>(' "',a8,'interval" "Fraction of volume" '))
       do jc=1,nacd_e
        if(cd_cnt(jc).gt.0.0)then
        cd_avg(jc)=cd_tot(jc)/cd_cnt(jc)
        else
        cd_avg(jc)=0.0
        end if

          do i=1,numclass
              if(i.eq.1)then
              conc_cd(jc,i)=cd_top(cdn_e(jc))
              else
              conc_cd(jc,i)=conc_cd(jc,i-1)-cd_int(cdn_e(jc))
              end if
          cd_sum(jc)=cd_sum(jc)+cd_class(jc,i)/sumvolt
          end do
       end do
        do i=1,numclass
        write(CONE,124)(conc_cd(jc,i),cd_class(jc,i)/sumvolt,jc=1,nacd_e)
        end do
        write(CONE,'(1x)')
        write(CONE,128)(cd_sum(jc),jc=1,nacd_e)
        write(CONE,'(1x)')
        write(CONE,128)(cd_avg(jc),jc=1,nacd_e)
        close(CONE)
    endif
 endif
       DEallocate(c_cnt,cd_cnt,c_class,cd_class,c_tot,cd_tot,t_class,v_class,c_sum,cd_sum)
       DEallocate(conc_c,conc_cd)
       DEallocate(cc_e,c_int,c_top,cd_e,cd_int,cd_top,c_avg,cd_avg,cn_e,cdn_e)
       
124       format(<nacd_e>(f6.2,3x,e12.4,3x))
125       format((f6.2,3x,e12.4,3x))
126       format(<nac_e>(f6.2,3x,e12.4,3x))
127       format(<nacd_e>(f6.2,3x,e12.4,3x))
128       format(<nacd_e>(" 0 ",e12.4))
129       format(<nac_e>(" 0 ",e12.4))

    end if

timlast=jday

      END SUBROUTINE ENVIRP

