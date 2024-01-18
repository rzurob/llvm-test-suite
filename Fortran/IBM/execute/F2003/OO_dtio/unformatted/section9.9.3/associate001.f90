!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - inquire iolength of scalar polymorphic items
!*                                 when output items are structure/array constructor
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type :: base
      integer(8)   :: x
      real(4)      :: y
      character(3) :: z
   end type

   type, extends(base) :: child
      integer(2) :: a
      real(8)    :: b
      complex(8) :: c
   end type

end module

program associate001
   use m1

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   procedure(integer) :: getIOlength

   associate ( a => base(x=1, y=2.2, z='abc'), b => child(1,2.3,'abc',4,5,(6.0,7.0)), c => (/ base(x=1, y=2.2, z='abc'), base(x=3, y=4.5, z='abc') /), &
               d => (/ ( child(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1,-2 ) /), e => (/ ( child(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1 ) /) )

      inquire ( iolength = length1 ) a
      if ( length1 /= 16 )            error stop 1_4

      inquire ( iolength = length1 ) b
      if ( length1 /= 48 )            error stop 2_4

      inquire ( iolength = length1 ) c
      if ( length1 /= 32 )            error stop 3_4

      inquire ( iolength = length1 ) d
      if ( length1 /= 240 )           error stop 4_4

      inquire ( iolength = length1 ) e
      if ( length1 /= 0 )             error stop 5_4

   end associate

end program
