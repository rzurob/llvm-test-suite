!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: save101.f
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
!*                                        Try namelist formatting with save attribute (input)
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

   type :: data
      integer :: j
   end type

   type :: base
      integer(4) :: i
      type(data) :: d
   end type

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

contains

   subroutine readDummy(unit)
      integer, intent(in) :: unit

      class(base), allocatable, save :: dummy
      class(base), allocatable, save :: olddummy

      namelist /nml/ dummy
      integer :: stat
      character(200) :: msg

      if ( .not. allocated(dummy) ) then
      	 allocate(dummy, source = base(100,data(200)) )
      	 allocate(olddummy, source = dummy )
      else
         read ( unit, nml, iostat=stat, iomsg = msg)
      	 if ( olddummy%i /= (dummy%i - 1 ) )          error stop 1_4
      	 if ( olddummy%d%j /= (dummy%d%j - 1 ) )      error stop 2_4
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

      	 olddummy%i = dummy%i
      	 olddummy%d%j = dummy%d%j

      end if

      print *, dummy%i, dummy%d%j

   end subroutine

end module

program save101
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'save101.1', form='formatted', access='stream' )

   call readDummy(1)
   call readDummy(1)
   call readDummy(1)
   call readDummy(1)
   call readDummy(1)

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   read (unit, "(I4)", iostat=iostat )              dtv%i
   if ( iostat /= 0 ) error stop 6_4
   read (unit, *, iostat= iostat )                  dtv%d

   iomsg = 'dtioread'

end subroutine

