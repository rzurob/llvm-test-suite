!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: scalar102.f
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
!*                                        Try namelist formatting inside DTIO procedure (input)
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
      character(3), pointer :: c => null()
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


program scalar102
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), allocatable  :: b4
   type(base), pointer      :: b5

   open (1, file = 'scalar102.1', form='formatted', access='sequential', status='old', BLANK='NULL' )

   allocate(b1, b2, b4, b5)

   allocate(b1%c, b2%c, b3%c, b4%c, b5%c)

   read  (1,*, iostat=stat, iomsg=msg)      b1
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read  (1,*, iostat=stat, iomsg=msg)      b2, b3
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read  (1,*, iostat=stat, iomsg=msg)      b4
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   read  (1,*, iostat=stat, iomsg=msg)      b5
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   if ( b1%c /= 'ABC' )  error stop 5_4
   if ( b2%c /= 'DEF' )  error stop 6_4
   if ( b3%c /= 'GHI' )  error stop 7_4
   if ( b4%c /= 'JKL' )  error stop 8_4
   if ( b5%c /= 'MNO' )  error stop 9_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   character(3), target :: c
   namelist /nml1/ c

   if ( iotype /= "LISTDIRECTED" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read (unit, nml1, iostat=iostat )

   allocate( dtv%c, source = c )

   iomsg = 'dtioread'

end subroutine
