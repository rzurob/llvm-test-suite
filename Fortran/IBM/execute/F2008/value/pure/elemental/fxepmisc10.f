! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qstrict -qrndsngl -qnomaf -qfixed=132
! %GROUP: fxepmisc10.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************

!*  ===================================================================
!*
!*  DATE                       : July 28, 1998
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ELEMENTAL procedures
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qstrict -qrndsngl -qnomaf
!*
!*  KEYWORD(S)                 : ELEMENTAL, array, parallel sections, derived
!*                               type, real, integer, character
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test calls to elemental functions
!*                               from within parallel sections. Here
!*                               the components of a derived type
!*                               are evaluated in parallel.
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

      PROGRAM fxepmisc10
      implicit none

      TYPE dt
        integer      int_sc
        integer      int_ar(10)
        real         real_sc
        real         real_ar(15)
        character*5  name
      END TYPE

      integer, parameter :: const=40
      TYPE(dt)   arr1(const),arr2(const),arr3(const),arr4(const)
      TYPE(dt)   res1(const),res2(const),res3(const),res4(const)
      integer count1, count2, count3, count4
      integer    iarr1(10),iarr2(10),iarr3(10)
      real       rarr1(15),rarr2(15),rarr3(15)

!*********************************************************************
!* Main program: Here we call elemental procedures.
!*********************************************************************

!* Initialize the helper arrays
      iarr1=(/ (count1,count1=1,10) /)
      iarr2=(/ (count1,count1=2,20,2) /)
      iarr3=(/ (count1,count1=10,100,10) /)

      rarr1=(/ (REAL(count1/2),count1=1,15) /)
      rarr2=(/ (REAL(count1/2),count1=3,45,3) /)
      rarr3=(/ (REAL(count1/2),count1=2,30,2) /)

!*********************************************************************
!* Initialize the derived type arrays within parallel sections
!* using elemental functions
!*********************************************************************


100   FORMAT(10I15,15E29.20,A5)

! Calculate the expeced values for the tests below

       res1%int_sc =15*(-12)-3
       res1%real_sc=1.5E0/13.25E0/21.0E0
       res1%name   ="chris"
       do count1=1,const
         res1(count1)%int_ar =iarr1*iarr2-iarr3
         res1(count1)%real_ar=rarr1/rarr2/rarr3
       end do

       res2%int_sc =5*12-(-13)
       res2%real_sc=15.5E0/132.5E0/2.10E0
       res2%name   ="vadim"
       do count1=1,const
         res2(count1)%int_ar =iarr2*iarr3-iarr1
         res2(count1)%real_ar=rarr2/(rarr3+1.0e0)/(rarr1+1.0e0)
       end do

       res3%int_sc =52*(-62)-73
       res3%real_sc=31.5E0/213.25E0/121.0E0
       res3%name   ="jenny"
       do count1=1,const
         res3(count1)%int_ar =iarr3*iarr1-iarr2
         res3(count1)%real_ar=rarr3/(rarr1+1.0e0)/(rarr2+1.0e0)
       end do

       res4%int_sc =-215*612-(-513)
       res4%real_sc=901.5E0/(-213.25E0)/(-217.0E0)
       res4%name   ="steve"
       do count1=1,const
         res4(count1)%int_ar =iarr3*iarr2-iarr1
         res4(count1)%real_ar=rarr3/(rarr2+1.0e0)/(rarr1+1.0e0)
       end do

!SMP$ PARALLEL SECTIONS, PRIVATE(count1)
!SMP$ SECTION
      do count1=1,const
        arr1(count1)%int_sc = elemfunc1(15,-12,3)
        arr1(count1)%int_ar = elemfunc1(iarr1,iarr2,iarr3)
        arr1(count1)%real_sc= elemfunc2(1.5E0,13.25E0,21.0E0)
        arr1(count1)%real_ar= elemfunc2(rarr1,rarr2,rarr3)
        arr1(count1)%name   = elemfunc3("chuck","marty","ellis")
      end do

!SMP$ SECTION
      do count1=1,const
        arr2(count1)%int_sc = elemfunc1(5,12,-13)
        arr2(count1)%int_ar = elemfunc1(iarr2,iarr3,iarr1)
        arr2(count1)%real_sc= elemfunc2(15.5E0,132.5E0,2.1E0)
        arr2(count1)%real_ar= elemfunc2(rarr2,rarr3+1.0e0,rarr1+1.0e0)
        arr2(count1)%name   = elemfunc3("valma","andie","hakim")
      end do

!SMP$ SECTION
      do count1=1,const
        arr3(count1)%int_sc = elemfunc1(52,-62,73)
        arr3(count1)%int_ar = elemfunc1(iarr3,iarr1,iarr2)
        arr3(count1)%real_sc= elemfunc2(31.5E0,213.25E0,121.0E0)
        arr3(count1)%real_ar= elemfunc2(rarr3,rarr1+1.0e0,rarr2+1.0e0)
        arr3(count1)%name   = elemfunc3("jenny","wendy","kenny")
      end do

!SMP$ SECTION
      do count1=1,const
        arr4(count1)%int_sc = elemfunc1(-215,612,-513)
        arr4(count1)%int_ar = elemfunc1(iarr3,iarr2,iarr1)
        arr4(count1)%real_sc= elemfunc2(901.5E0,-213.25E0,-217.0E0)
        arr4(count1)%real_ar= elemfunc2(rarr3,rarr2+1.0e0,rarr1+1.0e0)
        arr4(count1)%name   = elemfunc3("stan ","stewy","trove")
      end do

!SMP$ END PARALLEL SECTIONS

! Verify the results of the previous elemental calls
      call check(arr1,res1,1,const)
      call check(arr2,res2,2,const)
      call check(arr3,res3,3,const)
      call check(arr4,res4,4,const)



! Calculate the expected values for the next tests
      do count1=1,const
        res4(count1)%int_sc=arr4(count1)%int_sc
        res4(count1)%int_ar=arr4(count1)%int_ar
        res4(count1)%real_sc=arr4(count1)%real_sc
        res4(count1)%real_ar=arr4(count1)%real_ar
        res4(count1)%name=arr4(count1)%name
      end do

      do count1=1,const
        res4(count1)%int_sc=arr1(count1)%int_sc*arr2(count1)%int_sc-arr3(count1)%int_sc
        res4(count1)%real_sc=arr1(count1)%real_sc/arr2(count1)%real_sc/arr3(count1)%real_sc
        res4(count1)%name=arr1(count1)%name(1:2)//arr2(count1)%name(3:3)//arr3(count1)%name(4:5)
        res4(count1)%int_ar=arr1(count1)%int_ar*arr2(count1)%int_ar-arr3(count1)%int_ar
        res4(count1)%real_ar=arr1(count1)%real_ar/arr2(count1)%real_ar/arr3(count1)%real_ar
      end do

! Evaluate all of the components of an array section in parallel

!SMP$ PARALLEL SECTIONS, PRIVATE(count1)
!SMP$ SECTION
      do count1=1,const
        arr4(count1)%int_sc = elemfunc1(arr1(count1)%int_sc,
     +              arr2(count1)%int_sc,arr3(count1)%int_sc)
      end do

!SMP$ SECTION
      do count1=1,const
        arr4(count1)%int_ar = elemfunc1(arr1(count1)%int_ar,
     +              arr2(count1)%int_ar,arr3(count1)%int_ar)
      end do

!SMP$ SECTION
      do count1=1,const
        arr4(count1)%real_sc= elemfunc2(arr1(count1)%real_sc,
     +             arr2(count1)%real_sc,arr3(count1)%real_sc)
      end do

!SMP$ SECTION
      do count1=1,const
        arr4(count1)%real_ar= elemfunc2(arr1(count1)%real_ar,
     +             arr2(count1)%real_ar,arr3(count1)%real_ar)
      end do

!SMP$ SECTION
      do count1=1,const
        arr4(count1)%name   = elemfunc3(arr1(count1)%name,
     +                arr2(count1)%name,arr3(count1)%name)
      end do

!SMP$ END PARALLEL SECTIONS

! Verify the results of the previous test
      call check(arr4,res4,5,const)

      CONTAINS
!*********************************************************************
!* Elemental Function definitions
!*
!*********************************************************************
!*********************************************************************
!* elemfunc1:
!*********************************************************************

      ELEMENTAL INTEGER FUNCTION elemfunc1(A1,A2,A3)
       integer, value :: A1,A2,A3
       elemfunc1=A1*A2-A3
      END FUNCTION elemfunc1

!*********************************************************************
!* elemfunc2:
!*********************************************************************

      ELEMENTAL REAL FUNCTION elemfunc2(A1,A2,A3)
       real, value :: A1,A2,A3
       elemfunc2=A1/A2/A3
      END FUNCTION elemfunc2

!*********************************************************************
!* elemfunc3:
!*********************************************************************

      ELEMENTAL CHARACTER*5 FUNCTION elemfunc3(A1,A2,A3)
       character*5, value :: A1,A2,A3
       elemfunc3=A1(1:2)//A2(3:3)//A3(4:5)
      END FUNCTION elemfunc3

!*********************************************************************
!* elemfunc4:
!*********************************************************************

      ELEMENTAL FUNCTION elemfunc4(A1,A2,A3)
       TYPE(dt), value :: A1,A2,A3
       TYPE(dt)  elemfunc4
       elemfunc4%int_sc =A1%int_sc+A3%int_sc-A2%int_sc
       elemfunc4%int_ar = ((/A1%int_ar(1:5:2),A3%int_ar(2:5:2),A2%int_ar(2:10:2)/))
       elemfunc4%real_sc=A3%real_sc-A2%real_sc-A1%real_sc
       elemfunc4%real_ar=A1%real_ar/5.5E0*A2%real_ar/A3%real_ar
       elemfunc4%name   =A1%name(1:1)//A3%name(2:4)//A2%name(5:5)
      END FUNCTION elemfunc4


!*********************************************************************
!* Result checking subroutine
!*********************************************************************

      SUBROUTINE check(A1,A2,RC,SIZE)
       TYPE(dt) A1(:),A2(:)
       integer  RC,SIZE,count1,count2
       logical  precision_r4

       if (ANY(A1%name .ne. A2%name)) call zzrc(RC)
       if (ANY(A1%int_sc .ne. A2%int_sc)) call zzrc(RC)
       do count1=1,SIZE
         if (.not.precision_r4(A1(count1)%real_sc,A2(count1)%real_sc)) call zzrc(RC)
         do count2=1,10
           if (A1(count1)%int_ar(count2) .ne. A2(count1)%int_ar(count2)) call zzrc(RC)
         end do
         do count2=1,15
           if (.not. precision_r4(A1(count1)%real_ar(count2),A2(count1)%real_ar(count2))) call zzrc(RC)
         end do
       end do
      END SUBROUTINE check


!*********************************************************************
!* Main program ends
!*********************************************************************
      END
