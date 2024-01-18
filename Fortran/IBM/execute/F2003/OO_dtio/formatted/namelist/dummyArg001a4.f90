!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg001a4.f
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
!*                                        Try namelist formatting for derived type object which is a explicit array dummy argument
!*                                        which contains internal subroutine
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

   type :: base
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

   subroutine writeBase(dtv,lb)
      class(base), intent(in) :: dtv(3)
      integer, intent(in) :: lb

      integer :: stat
      character(200) :: msg

      if (( innerWriteBase(dtv,lb) /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

      contains

         integer function innerWriteBase(dtv,lb)
            class(base), intent(in) :: dtv(lb:(lb+2))  !<- contains 3 elements
            integer, intent(in) :: lb

            namelist /nml/ dtv
            write ( unit, nml, iostat=innerWriteBase, iomsg = msg)

         end function
   end subroutine

end module

program dummyArg001a4
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:)
   type(child)               :: b3(3)
   type(child), pointer      :: b4(:)

   open (unit, file = 'dummyArg001a4.1', form='formatted', access='stream' )

   allocate(b1(3), source = (/ base('abc'), base('def'), base('ghi') /) )
   allocate(b2(3), source = (/ ( child(c='IBM',i=j), j=1,3 ) /) )
   b3 = child(c='jkl', i=4)
   allocate(b4(3), source = (/ child(c='mno',i=5), child(c='pqr',i=6), child(c='stu',i=7) /) )

   call writeBase(b1,1)
   call writeBase(b2,2)
   call writeBase(b3,3)
   call writeBase(b4,4)

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
      type is ( base )
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is ( child )
         write (unit, "('i= ',I4,1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine
