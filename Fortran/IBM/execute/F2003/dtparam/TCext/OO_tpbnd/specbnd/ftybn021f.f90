! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_tpbnd/specbnd/ftybn021f.f
! opt variations: -qnol -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn021f.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn021f.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : multiple extends
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module mod
	 type base(n1,k1)    ! (20,4)
            integer, kind :: k1
            integer, len  :: n1
            integer(k1)   :: x
	 contains
   	    procedure, nopass :: bind1 => proc1
	 end type

      type, extends(base) :: parent    ! (20,4)
         integer(k1) :: y
      contains
         procedure, nopass :: bind2 => proc2
      end type

      type, extends(parent) :: child    ! (20,4)
         integer(k1) :: z
      contains
         procedure, nopass :: bind3 => proc3
      end type

      type, extends(child) :: granChild    ! (20,4)
         integer(k1) :: int
      contains
       	 procedure, nopass :: bind4 => proc4
      end type

      contains
      integer function proc1(arg1)
         class(base(*,4)), intent(in) :: arg1
         proc1 = arg1%x
      end function proc1

      integer function proc2(arg1)
         class(parent(*,4)), intent(in) :: arg1
         proc2 = arg1%y
      end function proc2

      integer function proc3(arg1)
         class(child(*,4)), intent(in) :: arg1
         proc3 = arg1%z
      end function proc3

      subroutine proc4(arg1, x, y, z)
         type(granChild(*,4)), intent(inout) :: arg1
         integer, intent(in) :: x, y, z
         arg1%x = x
         arg1%y = y
         arg1%z = z
         arg1%int = arg1%x + arg1%y + arg1%z
      end subroutine

   end module

   use mod

   type(granChild(20,4)) :: dt1, dt2
   dt1 = granChild(20,4)( 1, 2, 3, 6 )
   call dt2%bind4(dt2,1,2,3)

   if (dt1%x /= dt2%x)      error stop 2
   if (dt1%y /= dt2%y)      error stop 3
   if (dt1%z /= dt2%z)      error stop 4
   if (dt1%int /= dt2%int)  error stop 5

   if (dt1%bind1(dt1) /= 1)  error stop 6
   if (dt2%bind2(dt1) /= 2)  error stop 7
   if (dt2%bind3(dt1) /= 3)  error stop 8


   end

