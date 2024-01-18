!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: position101.f
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
!*                                        Try position edit descriptors (T, TL, TR, X) inside DTIO (Input)
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
      integer(4)   :: id
      character(3) :: name = ''
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

program position101
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type (base)              :: b3

   namelist /nml/ b1, b2, b3

   allocate( b1, source = base(101,'ibm'))
   allocate( b2, source = base(202,'ftn'))
   b3 = base ( 303, 'FTN')

   open (1, file = 'position101.1', form='formatted', access='sequential' )

   read (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( ( b1%name /= 'abc' ) .or. ( b1%id /= 101 ) ) error stop 2_4
   if ( ( b2%name /= 'def' ) .or. ( b2%id /= 102 ) ) error stop 3_4
   if ( ( b3%name /= 'ghi' ) .or. ( b3%id /= 103 ) ) error stop 4_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 5_4
   if ( size(v_list, 1) /= 0 ) error stop 6_4

   read ( unit, "(T9,I4,TL8,A3,/)", iostat = iostat )      dtv%id, dtv%name
   ! read one more time in reverse order
   read ( unit, "(T1, I4, TR1, 1X, A3)", iostat = iostat ) dtv%id, dtv%name

   iomsg = 'dtioread'

end subroutine

