!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array104.f
! %VERIFY: array104.1:array104.vf
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
!*                                        Try namelist formatting array objects with sequence type (input)
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
      sequence
      integer(4)   ::  i = -999
      character(3) ::  c = 'xxx'
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

program array104
   use m

   type(base), pointer     :: b1(:)
   type(base), allocatable :: b2(:)
   type(base)              :: b3(2,2)

   integer :: stat
   character(200) :: msg = ''

   namelist /nml1/ b1, b2, b3

   allocate ( b1(4), source = (/ base(), base(), base(), base() /) )
   allocate ( b2(4), source = (/ base(), base(), base(), base() /) )

   b3 = reshape( source = b2 , shape = (/2,2/) )

   open (1, file = 'array104.1', form='formatted', access='sequential' )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   print *, b1%i
   print *, b1%c
   print *, b2%i
   print *, b2%c
   print *, b3%i
   print *, b3%c

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   type(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   read ( unit, "(A3,1X)", iostat = iostat ) dtv%c
   read ( unit, "(I4,1X)", iostat = iostat ) dtv%i

   iomsg = 'dtioread'

end subroutine
