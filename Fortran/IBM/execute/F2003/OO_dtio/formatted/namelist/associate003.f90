!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate003.f
! %VERIFY: associate003.1:associate003.vf
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
!*                                        Try namelist formatting with associate construct
!*                                        change value with associate-name with unlimited polymorphic
!*                                        component(Output)
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
   type data
      integer :: i = 123
   end type

   type base
      class(*), pointer :: u
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

end module

program associate003
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), allocatable  :: b4
   type(base), pointer      :: b5

   namelist /nml/ b1, b2, b3
   namelist /nml/ b4, b5

   open (1, file = 'associate003.1', form='formatted', access='sequential' )
   allocate(b1, b2, b4, b5)

   associate( b1 => b2, b2 => b3, b3 => b4, b4 => b5, b5 => b1)
      allocate(b1%u, source = 5_8)
      allocate(b2%u, source = 'ibmftn')
      allocate(b3%u, source = data() )
      allocate(b4%u, source = 4_4)
      allocate(b5%u, source = 2_2)
      write (1, nml, iostat = stat, iomsg = msg )
   end associate

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, data

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(data) :: dummy
   namelist /dtio/ dummy

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (g => dtv%u)
      type is (integer(2))
         write ( 1, *, iostat = iostat ) g
      type is (integer(4))
         write ( 1, *, iostat = iostat ) g
      type is (integer(8))
         write ( 1, *, iostat = iostat ) g
      type is (character(*))
         write ( 1, *, iostat = iostat ) g
      type is (data)
         dummy = g
         write ( 1, dtio, iostat = iostat )
      class default
         error stop 4_4
   end select


   iomsg = 'dtiowrite'

end subroutine
