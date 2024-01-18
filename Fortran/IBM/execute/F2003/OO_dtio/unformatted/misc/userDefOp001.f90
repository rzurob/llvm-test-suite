!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: userDefOp001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment with DTIO
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type :: base
      integer(4) :: i1
   end type

   type, extends(base) :: child
      integer(4)   :: i2
   end type

   interface operator(+)
      class(base) function myAdd(a,b)
         import base, child
         class(base), intent(in) :: a, b
         allocatable :: myAdd
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn(a,b)
         import base, child
         class(base), intent(out) :: a
         class(base), intent(in)  :: b
      end subroutine
   end interface

end module

program userDefOp001
   use m

   class(base), allocatable :: b1, b2, b3, b4
   character(16) :: c1, c4
   character(1) :: c2, c3, c5, c6
   integer(4) :: i1, i2, i3
   integer(4) :: i4(2), i5(2), i6(2)

   open ( 1, file='userDefOp001.1', form='unformatted', access='sequential' )

   allocate ( b1, source = child ( 1000, 1001 ) )
   allocate ( b2, source = child ( 2000, 2001 ) )

   allocate ( child :: b3 )

   b3 = b1 + b2

   deallocate ( b1, b2 )

   allocate ( b1, source = base ( 100 ) )
   allocate ( b2, source = base ( 200 ) )

   allocate ( base :: b4 )

   b4 = b1 + b2

   rewind 1

   read (1) c4, i4, c5, i5, c6, i6
   read (1) c1, i1, c2, i2, c3, i3

   print *, c4, i4, c5, i5, c6, i6
   print *, c1, i1, c2, i2, c3, i3

   close ( 1, status = 'delete' )

end program

class(base) function myAdd(a,b)
   use m, only: base, child

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg
   class(base), intent(in) :: a, b
   allocatable :: myAdd

   if ( .not. same_type_as ( a, b ) ) error stop 1_4

   select type ( a )
      type is ( base )
         allocate ( myAdd, source = base ( i1= ( a%i1+b%i1 ) ) )
         write ( 1, iostat = stat, iomsg = msg ) "MyAdd = A + B : ", MyAdd, "=", A, "+", B
      type is ( child )
         select type ( b )
            type is ( child )
               allocate ( myAdd, source = child ( i1= ( a%i1+b%i1 ), i2= ( a%i2+b%i2 ) ) )
               write ( 1, iostat = stat, iomsg = msg ) "MyAdd = A + B : ", MyAdd, "=", A, "+", B
            class default
               error stop 2_4
         end select
   end select

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base), intent(out) :: a
   class(base), intent(in)  :: b

   if ( .not. same_type_as ( a, b ) ) error stop 3_4

   select type ( b )
      type is (base)
        a%i1 = b%i1
      type is (child)
        select type ( a )
           type is ( child )
              a%i1 = b%i1
              a%i2 = b%i2
           class default
              error stop 4_4
        end select
   end select

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%i1
      type is (child)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%i1, dtv%i2
   end select

   iomsg = 'dtiowrite'

end subroutine
