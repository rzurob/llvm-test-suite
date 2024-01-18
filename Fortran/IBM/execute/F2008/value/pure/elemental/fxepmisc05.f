! *********************************************************************

!*  ===================================================================
!*
!*  DATE                       : July 28, 1998
!*
!*  PRIMARY FUNCTIONS TESTED   : ELEMENTAL procedures
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qrndsngl -qstrict -qnomaf
!*
!*  KEYWORD(S)                 : ELEMENTAL, array, optional argument
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test elemental procedures with optional
!*                               dummy arguments.
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

      PROGRAM fxepmisc05
      implicit none
      integer count1, count2, count3, count4

      real*4, DIMENSION(5,2,5,2) :: D4arr21,D4arr22,D4arr23,D4arr24
     +                             ,D4arr25,D4arr26
      real*4, DIMENSION(40) :: D1arr21,D1arr22,D1arr23,D1arr24
      real*4, DIMENSION(2,1,2,1,1,1,1,1,2,1,1,2,1,1,3,2,2,3,2,1) ::
     +           D20arr21,D20arr22,D20arr23

!*********************************************************************
!* Main program: Here we call elemental procedures.
!*********************************************************************

      D4arr21=    55.0E1
      D4arr22=   556.5E2
      D4arr23=   456.0E0
      D4arr24=    79.5E-1
      D4arr25=     1.0E0
      D4arr26=  4235.5E0
      D4arr26(:,2,5,2)=  -132.0E0
      D1arr21=    99.5E0
      D1arr22=   100.0E-1
      D1arr23=  -182.5E1
      D1arr24=    30.5E0
      D20arr21=  -90.5E-1
      D20arr22=   68.0E0
      D20arr23= -103.5E1

      call sub1(D4arr21,D4arr22,D4arr23,D4arr24,D4arr25,D4arr26,D1arr21,
     +          D1arr22,D1arr23,D1arr24,D20arr21,D20arr22,D20arr23)

!*********************************************************************
!* Main program ends
!*********************************************************************
      END

      SUBROUTINE sub1(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)
      real*4, DIMENSION(5,2,5,2) :: A1,A2,A3,A4,A5,A6,res1,objective1
      real*4, DIMENSION(40) :: A7,A8,A9,A10,res2,objective2
      real*4, DIMENSION(2,1,2,1,1,1,1,1,2,1,1,2,1,1,3,2,2,3,2,1) ::
     +           A11,A12,A13,res3,objective3

      real*4  var1,var2,var3,var4,objective0
      real*4  tmp1(100),tmp2(100),tmp11(1152),tmp12(1152)
      integer   count1,count2
      logical   logarr1(5,2,5,2),logarr2(40),logarr3(2,1,2,1,1,1,1,1,2,1,1,2,1,1,3,2,2,3,2,1)
     +         ,precision_r4

      logarr1=.true.
      logarr2=.true.
      logarr3=.true.
      var1= -193.45E1
      var2=  13.5E0
      var3= -1893.045E2
      var4=  19.5E-1

100   FORMAT(E12.5,E12.5,E12.5,E12.5,E12.5)

! Calculate the expected values for the first test
      objective2=-99.0e0
      do count1=2,40,2
       objective2(count1)=A7(count1)*var2-A9(count1)*A10(count1)
      end do

      res2=-99.0e0
! Call elemental function with optional arg
      forall(count1=1:40,MOD(count1,2).eq.0)
       res2(count1)=elemfunc41(A1=A7(count1),A2=var2,A3=A9(count1),A4=A10(count1))
      end forall

! Verify the results of the previous test
      do count1=1,40
       if (.not. precision_r4(res2(count1),objective2(count1))) error stop 1
      end do

! Calculate the expected values for the second test
      objective1=-99.0e0
      forall(count1=1:5,count2=1:2)
       objective1(count1,count2,count1,count2)=(A4(count1,count2,count1,count2)+
     +                  A2(count1,count2,count1,count2))/A3(count1,count2,count1,count2)
      end forall

      res1=-99.0e0
! Call elemental function without optional arg
      forall(count1=1:5,count2=1:2)
       res1(count1,count2,count1,count2)=elemfunc41(A1=A4(count1,count2,count1,count2),
     +       A2=A3(count1,count2,count1,count2),A4=A2(count1,count2,count1,count2))
      end forall

! Verify the results of the previous test
      tmp1=RESHAPE(res1,(/100/))
      tmp2=RESHAPE(objective1,(/100/))
      do count1=1,100
       if (.not. precision_r4(tmp1(count1),tmp2(count1))) error stop 2
      end do

! Calculate the expected values for the third test
      where (logarr1 .EQV. .true.)
       objective1=A6+MAX(A1,A6,A2)-MIN(A1,A6,A2)
      end where

! Call elemental subroutine with optional arg
      call elemsub41(A1=A1,A2=A6,A3=A2)

! Verify the results of the previous test
      tmp1=RESHAPE(A6,(/100/))
      tmp2=RESHAPE(objective1,(/100/))
      do count1=1,100
       if (.not. precision_r4(tmp1(count1),tmp2(count1))) error stop 3
      end do

! Calculate the expected values for the fourth test
      where(logarr3 .EQV. .true.)
       objective3=SIN(A13)-A11
      end where

! Call elemental subroutine without optional arg
      call elemsub41(A1=A13,A2=A11)

! Verify the results of the previous test
      tmp11=RESHAPE(A11,(/1152/))
      tmp12=RESHAPE(objective3,(/1152/))
      do count1=1,1152
       if (.not. precision_r4(tmp11(count1),tmp12(count1))) error stop 4
      end do

      CONTAINS

!*********************************************************************
!* Elemental Function definitions
!*
!*********************************************************************
!*********************************************************************
!* elemfunc41:
!*********************************************************************

      ELEMENTAL REAL(4) FUNCTION elemfunc41(A1,A2,A3,A4)
       real*4, value :: A1,A2,A3,A4
       OPTIONAL :: A3

       if (PRESENT(A3)) then
         elemfunc41=A1*A2-A3*A4
       else
         elemfunc41=(A1+A4)/A2
       end if
      END FUNCTION elemfunc41

!*********************************************************************
!* Elemental Subroutine definitions
!*
!*********************************************************************
!*********************************************************************
!* elemsub41:
!*********************************************************************

      ELEMENTAL SUBROUTINE elemsub41(A1,A2,A3)
       real*4, value    :: A1
       real*4, OPTIONAL, value      :: A3
       real*4, INTENT(inout) :: A2
       if (present(A3)) then
         A2=A2+MAX(A1,A2,A3)-MIN(A1,A2,A3)
       else
         A2=SIN(A1)-A2
       end if
      END SUBROUTINE elemsub41

      END SUBROUTINE sub1
