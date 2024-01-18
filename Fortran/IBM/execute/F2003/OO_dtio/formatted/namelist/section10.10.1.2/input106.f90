!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: input106.f
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
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (dtio procedure involved, with array components)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
      integer(4)   :: i(3) = (/ -9, -9, -9 /)
   end type

   type, extends(base) :: child
      real(4)      :: r(3) = (/ -9.0, -9.0, -9.0 /)
   end type

   type, extends(child) :: gen3
      character(3) :: c(3) = (/ 'xxx', 'xxx', 'xxx' /)
   end type
end module

program input106
   use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
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
   character(150) :: msg = ''
   logical :: precision_r4

   class(base), allocatable  :: b1(:)
   class(base), pointer      :: b2(:,:)
   class(child), allocatable :: c1(:,:)
   class(child), pointer     :: c2(:)
   class(gen3), allocatable  :: g1(:,:,:)

   namelist /n1/ b1, c1, g1
   namelist /n2/ b2, c2

   allocate(b1(3))
   allocate(child :: b2(2,1), c1(2,3))
   allocate(gen3  :: c2(4), g1(2,2,2))

   open (1, file='input106.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   print *, stat, msg
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1, n2, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   print *, b1(1)%i
   print *, b1(2)%i
   print *, b1(3)%i
   select type(b2)
      type is (child)
         print *, b2(1,1)%i, b2(1,1)%r
         print *, b2(2,1)%i, b2(2,1)%r
      class default
         error stop 3_4
   end select

   print *, c1(1,1)%i, c1(1,1)%r
   print *, c1(2,1)%i, c1(2,1)%r
   print *, c1(1,2)%i, c1(1,2)%r
   print *, c1(2,2)%i, c1(2,2)%r
   print *, c1(1,3)%i, c1(1,3)%r
   print *, c1(2,3)%i, c1(2,3)%r

   select type(c2)
      type is (gen3)
         print *, c2(1)%i, c2(1)%r, c2(1)%c
         print *, c2(2)%i, c2(2)%r, c2(2)%c
         print *, c2(3)%i, c2(3)%r, c2(3)%c
         print *, c2(4)%i, c2(4)%r, c2(4)%c
      class default
         error stop 4_4
   end select

   print *, g1(1,1,1)%i, g1(1,1,1)%r, g1(1,1,1)%c
   print *, g1(2,1,1)%i, g1(2,1,1)%r, g1(2,1,1)%c
   print *, g1(1,2,1)%i, g1(1,2,1)%r, g1(1,2,1)%c
   print *, g1(2,2,1)%i, g1(2,2,1)%r, g1(2,2,1)%c
   print *, g1(1,1,2)%i, g1(1,1,2)%r, g1(1,1,2)%c
   print *, g1(2,1,2)%i, g1(2,1,2)%r, g1(2,1,2)%c
   print *, g1(1,2,2)%i, g1(1,2,2)%r, g1(1,2,2)%c
   print *, g1(2,2,2)%i, g1(2,2,2)%r, g1(2,2,2)%c

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   type(base) :: d1
   namelist /dtio1/ d1
   type(child) :: d2
   namelist /dtio2/ d2
   type(gen3) :: d3
   namelist /dtio3/ d3

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   select type(dtv)
      type is (base)
         read(unit, dtio1, iostat = iostat)
         dtv%i = d1%i
      type is (child)
         read(unit, dtio2, iostat = iostat)
         dtv%i = d2%i
         dtv%r = d2%r
      type is (gen3)
         read(unit, dtio3, iostat = iostat)
         dtv%i = d3%i
         dtv%r = d3%r
         dtv%c = d3%c
   end select

   iomsg = 'dtioread'

end subroutine
