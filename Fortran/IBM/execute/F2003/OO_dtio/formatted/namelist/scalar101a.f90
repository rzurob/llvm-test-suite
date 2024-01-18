!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: scalar101a.f
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
!*                                        Try namelist formatting with polymorphic/nonpoly scalar allocatable (Input)
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
      character(3), allocatable :: c
   end type

end module

module m1
   use m
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


program scalar101a
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), allocatable  :: b4
   type(base), pointer      :: b5

   namelist /nml/ b1, b2, b3, b4, b5
   open (1, file = 'scalar101a.1', form='formatted', access='sequential', status='old', BLANK='NULL' )

   allocate(b1, source = base(null()))
   allocate(b2, source = base(null()))
   allocate(b4, source = base(null()))
   allocate(b5, source = base(null()))

   allocate(b1%c, b2%c, b3%c, b4%c, b5%c)

   read  (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( b1%c /= 'xyz' ) error stop 2_4
   if ( b2%c /= 'def' ) error stop 3_4
   if ( b3%c /= 'jkl' ) error stop 4_4
   if ( b4%c /= 'mno' ) error stop 5_4
   if ( b5%c /= 'ghi' ) error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read (unit, "(A3,1X)", iostat=iostat )      dtv%c

   iomsg = 'dtioread'

end subroutine
