!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: input101.f
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
!*                                        Blanks are never used as zeros
!*                                        - inside DTIO, use data edit descriptor, make sure blanks can be used as zero
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
      integer(4) :: i
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

program input101
   use m

   integer :: stat
   character(150) :: msg
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), pointer      :: b4
   type(base), allocatable  :: b5

   namelist /n1/ b1, b2, b3
   namelist /n2/ b3, b4, b5

   allocate(b1, b2, b4, b5)

   open (1, file='input101.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)

   if ( ( b1%i /= 2000 ) .or. ( b2%i /= 1010 ) .or. ( b3%i /= 9009 ) ) error stop 1_4

   read (1, n2, iostat = stat, iomsg = msg)
   if ( ( b3%i /= 1002 ) .or. ( b4%i /= 1200 ) .or. ( b5%i /= 7007 ) ) error stop 2_4
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read (unit, "(I4)", iostat=iostat )  dtv%i

   iomsg = 'dtioread'

end subroutine
