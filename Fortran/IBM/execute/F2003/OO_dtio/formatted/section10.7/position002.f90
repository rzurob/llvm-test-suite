 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: position002.f
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
!*  DESCRIPTION                : Testing: Section 10.7.1: Position Editing
!*                                        Try different position editing descriptor in child data transfer stmt
!*                                        First try T, TL, TR, X edit descriptors together with I,A,F data editors
!*                                        with stream access
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

   type, abstract :: base
      real(4)      :: r
   end type

   type, extends(base) :: child
      integer(4)   :: i
      character(1) :: c
   end type

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program position002
   use m1

   ! declaration of variables

   procedure(logical) :: precision_r4
   class(base), allocatable :: f1
   type(child), pointer     :: f2(:)
   type(child) , allocatable :: f3
   class(base) , pointer     :: f4(:)

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (f1, source = child(1.0,2,'A'))
   allocate (f2(2), source = (/ child(3.333, 101, 'Z'), child(4.444, 202, 'X') /) )
   allocate (f3)
   allocate (child :: f4(2))

   open (unit = 1, file ='position002.1', form='formatted', access='stream')


   ! formatted I/O operations

   select type ( f11 => f1 )
      class is (child)
         write (1, *, iostat=stat, iomsg=msg)                f11
      class default
         error stop 1_4
   end select

   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )      error stop 2_4

   associate ( f22 => f2 )
      write (1, *, iostat=stat, iomsg=msg)                f22
      if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4
   end associate

   rewind 1

   associate ( f33 => f3 )
      read (1, *, iostat=stat, iomsg=msg)                 f33
   end associate

   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 4_4

   associate ( f44 => f4 )
      select type ( f444 => f44 )
         type is (child)
            read (1, *, iostat=stat, iomsg=msg)              f444
         class default
            error stop 5_4
      end select
   end associate

   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 6_4

   ! check if the values are read correctly

   if ( (.not. precision_r4(f3%r,1.0)) .or. ( f3%i /= 2 ) .or. (f3%c /= 'A' ) )  error stop 7_4

   select type ( f4 )
      type is (child)
         if ( (.not. precision_r4(f4(1)%r,3.333)) .or. ( f4(1)%i /= 101 ) .or. (f4(1)%c /= 'Z' ) .or. &
              (.not. precision_r4(f4(2)%r,4.444)) .or. ( f4(2)%i /= 202 ) .or. (f4(2)%c /= 'X' ) )  error stop 8_4
      class default
         error stop 9_4
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: child
   class(child), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( I3.0,1X,A1,TR1,F5.0 )

   read (unit, 10, iostat=iostat )          dtv%i, dtv%c, dtv%r

   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: child
   class(child), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( T1,I4.3,TR1,A1,1X,F5.3 )

   write (unit, 10, iostat=iostat )          dtv%i, dtv%c, dtv%r

   iomsg = 'dtiowrite'

end subroutine
