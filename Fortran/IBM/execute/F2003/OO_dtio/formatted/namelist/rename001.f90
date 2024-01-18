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
! %GROUP: rename001.f
! %VERIFY: rename001.1:rename001.vf
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
!*                                        Use Association with rename
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
   type base
      character(3) :: c
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

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   namelist /nml1/ b1, b2

end module


program rename001
   use m1, newbase => base, b11 => b1, b12 => b2

   namelist /nml1a/ b11, b12
   namelist /nml2/  b3, b4

   type (newbase) , allocatable :: b3
   class(newbase) , pointer     :: b4

   integer :: stat
   character(200) :: msg

   open (unit = 3, file ='rename001.1', form='formatted', access='sequential')

   ! allocation of variables

   allocate (b11, source = newbase('abc') )
   allocate (b12, source = newbase('def') )
   allocate (b3, source = newbase('ghi') )
   allocate (b4, source = newbase('ghi') )

   ! unformatted I/O operations

   write ( 3, nml1, iostat = stat, iomsg = msg )
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 1_4
   write ( 3, nml1a, iostat = stat, iomsg = msg )
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 2_4
   write ( 3, nml2, iostat = stat, iomsg = msg )
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "('c= ', A3, 1X)", iostat=iostat )        dtv%c

   iomsg = 'dtiowrite'

end subroutine
