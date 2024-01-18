!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg001a1.f
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
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object
!*                                        which is a explicit array dummy argument with non-initialization
!*                                        expr bounds.
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

   type, abstract :: base
      character(3) ::  c
   end type

   type, extends(base) :: child
      integer(4)   ::  i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: unit = 1

contains

   subroutine writeBase(dtv,lb,ub)
      integer, intent(in) :: lb,ub
      class(base), intent(in) :: dtv(lb:ub)
      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv
      write ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program dummyArg001a1
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable  :: b1(:)
   class(base), pointer      :: b2(:)
   type(child)               :: b3(3)
   type(child), pointer      :: b4(:)

   open (unit, file = 'dummyArg001a1.1', form='formatted', access='stream' )

   allocate(b1(2:4), source = (/ child('abc',1), child('def',2), child('ghi',3) /) )
   allocate(b2(3:5), source = (/ ( child(c='IBM',i=j), j=1,3 ) /) )
   b3 = child('jkl',4)
   allocate(b4(4:6), source = (/ child('mno',5), child('pqr',6), child('stu',7) /) )

   call writeBase(b1,1,3)
   call writeBase(b2,2,4)
   call writeBase(b3,3,5)
   call writeBase(b4,4,6)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type ( dtv )
      class is ( base )
         error stop 4_4
      type is ( child )
         write (unit, "('i= ',I4,1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine

