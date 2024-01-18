!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: userDefOp101.f
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
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
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

program userDefOp101
   use m

   class(base), allocatable :: b1, b2, b3, b4

   allocate ( child :: b1, b2 )
   allocate ( child :: b3 )

   open ( 1, file='userDefOp101.1', form='formatted', access='sequential' )

   b3 = b1 + b2

   select type ( b1 )
      type is ( child )
         if ( ( b1%i1 /= 1000 ) .or. ( b1%i2 /= 1001 ) ) error stop 1_4
      class default
         error stop 2_4
   end select

   select type ( b2 )
      type is ( child )
         if ( ( b2%i1 /= 2000 ) .or. ( b2%i2 /= 2002 ) ) error stop 3_4
      class default
          error stop 4_4
   end select

   select type ( b3 )
      type is ( child )
         if ( ( b3%i1 /= 3000 ) .or. ( b3%i2 /= 3003 ) ) error stop 5_4
      class default
         error stop 6_4
   end select

   deallocate ( b1, b2 )
   allocate ( base :: b1, b2 )
   allocate ( base :: b4 )

   b4 = b1 + b2

   select type ( b1 )
      type is ( base )
         if ( ( b1%i1 /= 100 ) ) error stop 7_4
      class default
         error stop 8_4
   end select

   select type ( b2 )
      type is ( base )
         if ( ( b2%i1 /= 200 ) ) error stop 9_4
      class default
         error stop 10_4
   end select

   select type ( b4 )
      type is ( base )
         if ( ( b4%i1 /= 300 ) ) error stop 11_4
      class default
         error stop 12_4
   end select

end program


class(base) function myAdd(a,b)
   use m, only: base, child

   interface read(formatted)
      subroutine readFormatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg
   class(base), intent(inout) :: a, b
   allocatable :: myAdd
   namelist /myadd1/ a, b

   if ( .not. same_type_as ( a, b ) ) error stop 13_4

   select type ( a )
      type is ( base )
         read ( 1, myadd1, iostat = stat, iomsg = msg )
         allocate ( myAdd, source = base ( i1= ( a%i1+b%i1 ) ) )
      type is ( child )
         select type ( b )
            type is ( child )
               read ( 1, myadd1, iostat = stat, iomsg = msg )
               allocate ( myAdd, source = child ( i1= ( a%i1+b%i1 ), i2= ( a%i2+b%i2 ) ) )
            class default
               error stop 14_4
         end select
   end select

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base), intent(out) :: a
   class(base), intent(in)  :: b

   if ( .not. same_type_as ( a, b ) ) error stop 15_4

   select type ( b )
      type is (base)
        a%i1 = b%i1
      type is (child)
        select type ( a )
           type is ( child )
              a%i1 = b%i1
              a%i2 = b%i2
           class default
              error stop 16_4
        end select
   end select

end subroutine

subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m
   class(base), intent(inout) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         read (unit, "(I3)", iostat=iostat, iomsg=iomsg )       dtv%i1
      type is (child)
         read (unit, "(I4,1X,I4)", iostat=iostat, iomsg=iomsg ) dtv%i1, dtv%i2
   end select

   iomsg = 'dtiowrite'

end subroutine
