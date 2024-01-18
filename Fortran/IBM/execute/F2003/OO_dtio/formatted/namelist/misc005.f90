!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: misc005.f
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
!*                               child write statement writing to units that is NOT the same as the unit specified by parent write statement
!*                               - parent specifies external file, and child specify internal file
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
      integer(4) :: i = -999
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

   character(10) :: internalFile(10)

end module

program misc005
   use m

   integer :: stat
   character(150) :: msg =''
   class(base), allocatable :: b1

   namelist /nml/ b1

   allocate ( b1, source = base(1001) )

   open (1, file='misc005.1', form='formatted', access='sequential' )

   write (1, nml, iostat = stat, iomsg = msg)

   print *, internalFile

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, internalFile

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   write (unit, "('i= ',I4)", iostat=iostat )          dtv%i
   write (internalFile, "('i= ',I4)", iostat=iostat )  dtv%i

   iomsg = 'dtiowrite'

end subroutine
