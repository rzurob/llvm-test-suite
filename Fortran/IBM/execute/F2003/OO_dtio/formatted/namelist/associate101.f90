!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate101.f
! %VERIFY: associate101.1:associate101.vf
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
!*                                        Try namelist formatting with associate construct (Input)
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
      integer :: i
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

end module

program associate101
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base)               :: b3
   type(base), allocatable  :: b4

   namelist /nml/ b1, b3, b4

   open (1, file = 'associate101.1', form='formatted', access='sequential' )
   allocate(b1, b4)

   b1%i = 0
   b3%i = 0
   b4%i = 0

   associate ( b1 => b3 , b3 => b4, b4 => b1 )

      read (1,NML=nml, iostat=stat, iomsg=msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      if ( b4%i /= 1234 ) error stop 2_4
      if ( b1%i /= 2345 ) error stop 3_4
      if ( b3%i /= 3456 ) error stop 4_4
   end associate

   if ( b1%i /= 1234 ) error stop 5_4
   if ( b3%i /= 2345 ) error stop 6_4
   if ( b4%i /= 3456 ) error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   read (unit, "(I4)", iostat=iostat )      dtv%i

   iomsg = 'dtioread'

end subroutine
