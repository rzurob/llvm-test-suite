!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg101a3.f
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
!*                                        Try namelist formatting for derived type object which is a assumed-shape array dummy argument
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
      character(3) ::  c ='xxx'
   end type

   type, extends(base) :: child
      integer(4)   ::  i =-999
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

   class(base), pointer :: b2(:)

contains

   subroutine readBase1(dtv, unit)
      class(base), intent(inout), allocatable :: dtv(:)
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

   subroutine readBase(dtv, unit)
      class(base), intent(inout), pointer :: dtv(:)
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   end subroutine

end module

program dummyArg101a3
   use m

   integer :: stat
   character(200) :: msg = ''

   class(base), allocatable  :: b1(:)
   class(base), allocatable   :: b3(:)
   class(base), pointer      :: b4(:)

   open (1, file = 'dummyArg101a3.1', form='formatted', access='stream' )

   allocate( child :: b1(2) )
   allocate( child :: b2(2) )
   allocate( b3(2) )
   allocate( child :: b4(4) )

   call readBase1(b1,1)
   call readBase (b2,1)
   call readBase1(b3,1)
   call readBase (b4,1)

   select type ( b1 )
      type is ( base )
         error stop 2_4
      type is ( child )
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 1 ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 2 ) )    error stop 3_4
   end select

   select type ( b2 )
      type is ( base )
         error stop 4_4
      type is ( child )
         if ( ( b2(1)%c /= 'ghi' ) .or. ( b2(1)%i /= 3 ) .or. ( b2(2)%c /= 'jkl' ) .or. ( b2(2)%i /= 4 ) ) error stop 5_4
   end select

   if ( ( b3(1)%c /= 'mno' ) .or. ( b3(2)%c /= 'pqr' ) ) error stop 6_4

   select type ( b4 )
      type is ( base )
         error stop 7_4
      type is ( child )
         if ( ( b4(1)%c /= 'stu' ) .or. ( b4(1)%i /= 4 ) .or. ( b4(2)%c /= 'vwx' ) .or. ( b4(2)%i /= 5 ) .or. &
              ( b4(3)%c /= 'xxx' ) .or. ( b4(3)%i /= -999 ) .or. ( b4(4)%c /= 'xxx' ) .or. ( b4(4)%i /= -999 ) )    error stop 8_4
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   select type (dtv)
      type is (base)
         read (unit, "(A3)", iostat=iostat )          dtv%c
      type is (child)
         read (unit, *, iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine

