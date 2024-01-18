! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qrndsngl -qstrict -qnomaf -qfixed=132
! %GROUP: fxepmisc07.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************

!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            : User Defined Elemental Procedures
!*                                                                     
!*  PROGRAMMER                 : Chris Hayes 
!*  DATE                       : July 28, 1998
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : ELEMENTAL procedures
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : -qrndsngl -qstrict -qnomaf
!*
!*  KEYWORD(S)                 : ELEMENTAL, array, genereic interface
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Tests elemental functions accessed
!*                               via a generic interface block.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  07/28/98   CH     -Initial release
!*  12/01/10   GB     -Copy from $(tsrcdir)elemental/epmisc/*.f - feature 384867
!*                     changing intent(in) to value for non arrays dummy args
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      PROGRAM fxepmisc07
      implicit none
      integer count1, count2, count3, count4

      integer*4  Ivar1,Ivar2,Ivar3,Ivar4,objective0
      integer*4, DIMENSION(50) ::  Iarr1, Iarr2, Iarr3, Iarr4, Ires1, objective1
      integer*4, DIMENSION(5,10) ::  Iarr11, Iarr12, Iarr13, Iarr14, Ires11, objective2
      real*4     Rvar1,Rvar2,Rvar3,Rres0,objective10
      real*4,    DIMENSION(50) ::  Rarr1, Rarr2, Rarr3, Rarr4, Rres1, objective11
      real*4,    DIMENSION(5,10) ::  Rarr11, Rarr12, Rarr13, Rarr14, Rres11, objective12
      complex    Xvar1,Xvar2,Xvar3,Xres0,objective20
      complex  , DIMENSION(50) ::  Xarr1, Xarr2, Xarr3, Xarr4, Xres1, objective21
      complex  , DIMENSION(5,10) ::  Xarr11, Xarr12, Xarr13, Xarr14, Xres11, objective22
      logical    precision_x8, precision_r4, logarr1(50), logarr2(10,5)
      

      INTERFACE func
        ELEMENTAL INTEGER(4) FUNCTION elem_int(A1,A2,A3)
         integer*4, value :: A1,A2,A3
        END FUNCTION elem_int
  
        ELEMENTAL REAL(4) FUNCTION elem_real(A1,A2,A3)
         real*4, value :: A1,A2,A3
        END FUNCTION elem_real

        ELEMENTAL COMPLEX FUNCTION elem_cpx(A1,A2,A3)
         complex, value :: A1,A2,A3
        END FUNCTION elem_cpx

        ELEMENTAL COMPLEX FUNCTION elem_mix(A1,A2,A3)
         complex  , value :: A1
         real*4   , value :: A2
         integer*4, value :: A3
        END FUNCTION elem_mix

        FUNCTION func_int(A1,A2,A3)
         integer*4 A1,A2(2),A3,func_int(2)
        END FUNCTION func_int

        FUNCTION func_real(A1,A2,A3)
         real*4, DIMENSION(7) :: A1,A2,func_real
         real*4  A3
         integer count1
        END FUNCTION func_real

        FUNCTION func_cpx(A1,A2,A3)
         complex A1(5,10),A2(5,10),A3(5,10),func_cpx(5,10)
         integer count1, count2
        END FUNCTION func_cpx

        FUNCTION func_mix(A1,A2,A3)
         complex   A1(50), func_mix(50)
         real*4    A2(50)
         integer*4 A3
        END FUNCTION func_mix
      END INTERFACE

!*********************************************************************
!* Main program: Here we call elemental procedures.
!*********************************************************************

      logarr1=.true.
      logarr2=.true.

      Iarr1= (/ ((-1)**count1,count1=1,50) /)
      Iarr2= (/ (count1,count1=1,50) /)
      Iarr3= (/ (20+(-1)**count1,count1=1,50) /)
      Iarr4= (/ (30-count1,count1=1,50) /)
      Iarr11=75
      Iarr12=85
      Iarr13=95
      Iarr14=105
      Ivar1= 23
      Ivar2= 33
      Ivar3= 43
      Ivar4= 53

      Rarr1= (/ (REAL(count1,4)/2.5E0,count1=1,99,2) /)
      Rarr2= (/ ((-1)**count1*REAL(count1,4)/2.5E0,count1=1,50) /)
      Rarr3= (/ (10.0E0-REAL(count1,4)/2.5E0,count1=1,50) /)
      Rarr4= (/ (7.5E0+(-1)**count1*REAL(count1,4)/2.5E0,count1=1,50) /)
      Rarr11=17.5E0
      Rarr12=28.0E0
      Rarr13=39.5E0
      Rarr14=51.0E0
      Rvar1=1.5E-7
      Rvar2=2.5E-7
      Rvar3=9.5E-7

      Xarr1(1:50:2)=(5.5E0,10.0E0)
      Xarr1(2:50:2)=(25.5E0,10.0E0)
      Xarr2(1:50:2)=(15.5E0,10.0E0)
      Xarr2(2:50:2)=(1.5E0,10.0E0)
      Xarr3(1:50:2)=(-5.5E0,10.0E0)
      Xarr3(2:50:2)=(15.5E0,10.0E0)
      Xarr4(1:50:2)=(-15.5E0,10.0E0)
      Xarr4(2:50:2)=(-1.5E0,10.0E0)
      Xarr11(:,1:10:2)=(95.5E0,-10.0E0)
      Xarr11(:,2:10:2)=(925.5E0,-1.0E0)
      Xarr12(:,1:10:2)=(915.5E0,-10.0E0)
      Xarr12(:,2:10:2)=(91.5E0,-1.0E0)
      Xarr13(:,1:10:2)=(-95.5E0,-10.0E0)
      Xarr13(:,2:10:2)=(195.5E0,-1.0E0)
      Xarr14(:,1:10:2)=(-915.5E0,-10.0E0)
      Xarr14(:,2:10:2)=(-19.5E0,-1.0E0)
      Xvar1=(7.5E0,7.5E0)
      Xvar2=(107.5E0,107.5E0)
      Xvar3=(10107.5E0,10107.5E0)


! Test the operator on integer data types
      Ires1(1:2)=func(Ivar1,(/Ivar3,Ivar3/),70)
      objective1(1:2)=Ivar1*(/Ivar3,Ivar3/)-(70+55)
      if (ANY(Ires1(1:2) .ne. objective1(1:2))) error stop 1

      Ires1=func(Iarr2,Iarr4,Iarr3)
      objective1=Iarr2+Iarr4+Iarr3+5000
      if (ANY(Ires1 .ne. objective1)) error stop 2

      Ires11=func(Iarr11,Iarr13,Iarr14)
      objective2=Iarr11+Iarr13+Iarr14+5000
      if (ANY(Ires11 .ne. objective2)) error stop 3

      Ires1(1:7)=func(Iarr1(11:17),Iarr4(2:8),Iarr2(1:7))
      objective1(1:7)=Iarr1(11:17)+Iarr4(2:8)+Iarr2(1:7)+5000
      if (ANY(Ires1(1:7) .ne. objective1(1:7))) error stop 4

      Ires1(1:3)=func(Ivar1,Ivar2,(/Ivar3,Ivar2,Ivar1/))
      objective1(1:3)=Ivar1+Ivar2+(/Ivar3,Ivar2,Ivar1/)+5000
      if (ANY(Ires1(1:3) .ne. objective1(1:3))) error stop 5

      Ires1(1:1)=func(Ivar1,Ivar2,(/Ivar3/))
      objective1(1:1)=Ivar1+Ivar2+(/Ivar3/)+5000
      if (ANY(Ires1(1:1) .ne. objective1(1:1))) error stop 6

! Test the operator on real data types
      Rres1(1:7)=func(Rarr1(8:14),Rarr3(1:7),Rvar2)
      objective11(1:7)=-Rarr1(8:14)/Rarr3(1:7)*Rvar2
      do count1=1,7
        if (.not. precision_r4(Rres1(count1),objective11(count1))) error stop 11
      end do

      Rres11=func(Rarr12,Rvar2,Rarr13)
      objective12=1.0E7-Rarr12-Rvar2-Rarr13
      do count1=1,5
       do count2=1,10
        if (.not. precision_r4(Rres11(count1,count2),
     +                    objective12(count1,count2))) error stop 12
       end do
      end do

      Rres1=func(Rarr1,Rarr2,Rarr3)
      objective11=1.0E7-Rarr1-Rarr2-Rarr3
      do count1=1,50
        if (.not. precision_r4(Rres1(count1),objective11(count1))) error stop 13
      end do

      Rres1(1:7)=func(Rarr1(8:14),Rvar3,Rarr3(1:7))
      objective11(1:7)=1.0E7-Rarr1(8:14)-Rvar3-Rarr3(1:7)
      do count1=1,7
        if (.not. precision_r4(Rres1(count1),objective11(count1))) error stop 14
      end do

      Rres1(1:7)=func(Rarr1(1:7),Rarr2(1:7),Rarr3(1:7))
      objective11(1:7)=1.0E7-Rarr1(1:7)-Rarr2(1:7)-Rarr3(1:7)
      do count1=1,7
        if (.not. precision_r4(Rres1(count1),objective11(count1))) error stop 15
      end do

      Rres1(1:8)=func(Rarr1(1:8),Rarr2(1:8),Rarr4(1:8))
      objective11(1:8)=1.0E7-Rarr1(1:8)-Rarr2(1:8)-Rarr4(1:8)
      do count1=1,8
        if (.not. precision_r4(Rres1(count1),objective11(count1))) error stop 16
      end do

      Rres0=func(Rvar2,Rvar1,Rvar3)
      objective10=1.0E7-Rvar2-Rvar1-Rvar3
      if (.not. precision_r4(Rres0,objective10)) error stop 17


! Test the operator on complex data types
      Xres11=func(Xarr11,Xarr13,Xarr14)
      objective22=CONJG(Xarr11-Xarr13+Xarr14)
      do count1=1,5
       do count2=1,10
        if (.not. precision_x8(Xres11(count1,count2),
     +                    objective22(count1,count2))) error stop 21
       end do
      end do

      Xres1=func(Xarr1,Xarr2,Xarr3)
      objective21=(1.0E7,1.0E7)-Xarr1-Xarr2-Xarr3
      do count1=1,50
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 22
      end do

      Xres1=func(Xvar1,Xvar2,Xarr3)
      objective21=(1.0E7,1.0E7)-Xvar1-Xvar2-Xarr3
      do count1=1,50
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 23
      end do

      Xres11=func(Xarr11,Xvar2,Xarr13)
      objective22=(1.0E7,1.0E7)-Xarr11-Xvar2-Xarr13
      do count1=1,5
       do count2=1,10
        if (.not. precision_x8(Xres11(count1,count2),
     +                    objective22(count1,count2))) error stop 24
       end do
      end do

      Xres1(1:2)=func(Xarr12(1,5:6),Xarr13(1,7:8),Xarr14(1,9:10))
      objective21(1:2)=(1.0E7,1.0E7)-Xarr12(1,5:6)-Xarr13(1,7:8)-Xarr14(1,9:10)
      do count1=1,2
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 25
      end do

      Xres0=func(Xvar1,Xvar3,Xvar2)
      objective20=(1.0E7,1.0E7)-Xvar1-Xvar3-Xvar2
      if (.not. precision_x8(Xres0,objective20)) error stop 26


! Test the operator on a mix of the data types
      Xres1=func(Xarr1,Rarr4,Ivar3)
      objective21=Xarr1-CMPLX(Ivar3*Rarr4,Ivar3)
      do count1=1,50
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 31
      end do

      Xres1=func(Xarr2,Rarr3,Iarr1)
      objective21=(1.0E7,1.0E7)-CONJG(Xarr2+CMPLX(Iarr1*Rarr3,Iarr1))
      do count1=1,50
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 32
      end do

      Xres1=func(Xvar2,Rarr2,Iarr3)
      objective21=(1.0E7,1.0E7)-CONJG(Xvar2+CMPLX(Iarr3*Rarr2,Iarr3))
      do count1=1,50
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 33
      end do

      Xres11=func(Xarr14,Rvar3,Iarr12)
      objective22=(1.0E7,1.0E7)-CONJG(Xarr14+CMPLX(Iarr12*Rvar3,Iarr12))
      do count1=1,5
       do count2=1,10
        if (.not. precision_x8(Xres11(count1,count2),
     +                   objective22(count1,count2))) error stop 34
       end do
      end do

      Xres11=func(Xarr11,Rarr12,Iarr11)
      objective22=(1.0E7,1.0E7)-CONJG(Xarr11+CMPLX(Iarr11*Rarr12,Iarr11))
      do count1=1,5
       do count2=1,10
        if (.not. precision_x8(Xres11(count1,count2),
     +                   objective22(count1,count2))) error stop 35
       end do
      end do

      Xres1=func(Xvar1,Rarr3,Ivar2)
      objective21=(1.0E7,1.0E7)-CONJG(Xvar1+CMPLX(Ivar2*Rarr3,Ivar2))
      do count1=1,50
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 36
      end do

      Xres1=func(Xarr3,Rvar1,Ivar4)
      objective21=(1.0E7,1.0E7)-CONJG(Xarr3+CMPLX(Ivar4*Rvar1,Ivar4))
      do count1=1,50
        if (.not. precision_x8(Xres1(count1),objective21(count1))) error stop 37
      end do

      Xres0=func(Xvar1,Rvar2,Ivar3)
      objective20=(1.0E7,1.0E7)-CONJG(Xvar1+CMPLX(Ivar3*Rvar2,Ivar3))
      if (.not. precision_x8(Xres0,objective20)) error stop 38



!*********************************************************************
!* Main program ends
!*********************************************************************
      END 


!*********************************************************************
!* Elemental Function definitions
!* 
!*********************************************************************
!*********************************************************************
!* elem_int: 
!*********************************************************************

      ELEMENTAL INTEGER(4) FUNCTION elem_int(A1,A2,A3)
       integer*4, value :: A1,A2,A3
       elem_int=A2+A1+A3+5000
      END FUNCTION elem_int

!*********************************************************************
!* elem_real: 
!*********************************************************************

      ELEMENTAL REAL(4) FUNCTION elem_real(A1,A2,A3)
       real*4, value :: A1,A2,A3
       elem_real=1.0E7-A1-A2-A3
      END FUNCTION elem_real

!*********************************************************************
!* elem_cpx: 
!*********************************************************************

      ELEMENTAL COMPLEX FUNCTION elem_cpx(A1,A2,A3)
       complex, value :: A1,A2,A3
       elem_cpx=(1.0E7,1.0E7)-A1-A2-A3
      END FUNCTION elem_cpx

!*********************************************************************
!* elem_mix: 
!*********************************************************************

      ELEMENTAL COMPLEX FUNCTION elem_mix(A1,A2,A3)
       complex  , value :: A1
       real*4   , value :: A2
       integer*4, value :: A3
       elem_mix=(1.0E7,1.0E7)-CONJG(A1+CMPLX(A3*A2,A3))
      END FUNCTION elem_mix

!*********************************************************************
!* func_int: takes in 3 int scalars
!*********************************************************************

      FUNCTION func_int(A1,A2,A3)
       integer*4 A1,A2(2),A3,func_int(2)
       func_int=A1*A2-(A3+55)
      END FUNCTION func_int

!*********************************************************************
!* func_real: takes in 3 arrays of size 7, rank 1
!*********************************************************************

      FUNCTION func_real(A1,A2,A3)
       real*4, DIMENSION(7) :: A1,A2,func_real
       real*4 A3
       integer count1
       do count1=1,7
        func_real(count1)=-A1(count1)/A2(count1)*A3
       end do
      END FUNCTION func_real

!*********************************************************************
!* func_cpx: takes in 3 arrays of rank 2, size 50 (5X10)
!*********************************************************************

      FUNCTION func_cpx(A1,A2,A3)
       complex A1(5,10),A2(5,10),A3(5,10),func_cpx(5,10)
       integer count1, count2
       do count1=1,5
        do count2=1,10
         func_cpx(count1,count2)=CONJG(A1(count1,count2)-A2(count1,count2)
     +                +A3(count1,count2))
        end do
       end do
      END FUNCTION func_cpx

!*********************************************************************
!* func_mix:  takes in an array, a scalar and an array
!*********************************************************************

      FUNCTION func_mix(A1,A2,A3)
       complex   A1(50), func_mix(50)
       integer*4 A3, count1
       real*4    A2(50)
       do count1=1,50
         func_mix(count1)= A1(count1)-CMPLX(A3*A2(count1),A3)
       end do
       return
      END FUNCTION func_mix
