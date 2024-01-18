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
! %GROUP: misc103.f
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
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with sequence type (input)
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
      sequence
      integer :: i(2)
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program misc103
   use m

   integer :: stat
   character(200) :: msg = ''
   type(base)               :: b1
   type(base), pointer      :: b2
   type(base), allocatable  :: b3

   namelist /nml/  b1, b2
   namelist /nml1/ b1, b2, b3

   open (1, file = 'misc103.1', form='formatted', access='sequential' )
   allocate(b2, b3)

   b1%i = 0
   b2%i = 0
   b3%i = 0

   read (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

   if ( ( b1%i(1)  /= 1 ) .or. ( b1%i(2)  /= 2 ) )  error stop 2_4
   if ( ( b2%i(1)  /= 3 ) .or. ( b2%i(2)  /= 4 ) )  error stop 3_4

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) )  error stop 4_4

   if ( ( b1%i(1)  /= 5 ) .or. ( b1%i(2)  /= 6 ) )  error stop 5_4
   if ( ( b2%i(1)  /= 7 ) .or. ( b2%i(2)  /= 8 ) )  error stop 6_4
   if ( ( b3%i(1)  /= 9 ) .or. ( b3%i(2)  /= 10 ) ) error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   type(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   namelist /dtio/ dtv

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   read (unit, dtio, iostat=iostat )

   iomsg = 'dtioread'

end subroutine
