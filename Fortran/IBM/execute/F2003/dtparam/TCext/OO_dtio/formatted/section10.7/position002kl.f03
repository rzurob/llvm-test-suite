! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-22 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type, abstract :: base  (kb) ! kb=4
      integer, kind :: kb
      real(kb)      :: r
   end type

   type, extends(base) :: child (kc,lc) ! kc,lc=4,1
      integer, kind :: kc
      integer, len :: lc
      integer(kc)   :: i
      character(lc) :: c
   end type

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child(4,4,*)), intent(in) :: dtv ! tcx: (4,4,*)
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
         class(child(4,4,*)), intent(inout) :: dtv ! tcx: (4,4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program position002kl
   use m1

   ! declaration of variables

   procedure(logical) :: precision_r4
   class(base(4)), allocatable :: f1 ! tcx: (4)
   type(child(4,4,:)), pointer     :: f2(:) ! tcx: (4,4,:)
   type(child(4,4,:)) , allocatable :: f3 ! tcx: (4,4,:)
   class(base(4)) , pointer     :: f4(:) ! tcx: (4)

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (f1, source = child(4,4,1)(1.0,2,'A')) ! tcx: (4,4,1)
   allocate (f2(2), source = (/ child(4,4,1)(3.333, 101, 'Z'), child(4,4,1)(4.444, 202, 'X') /) ) ! tcx: (4,4,1) ! tcx: (4,4,1)
   allocate (child(4,4,1):: f3) ! tcx: child(4,4,1)
   allocate (child(4,4,1) :: f4(2)) ! tcx: (4,4,1)

   open (unit = 1, file ='position002kl.1', form='formatted', access='stream')


   ! formatted I/O operations

   select type ( f11 => f1 )
      class is (child(4,4,*)) ! tcx: (4,4,*)
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
         type is (child(4,4,*)) ! tcx: (4,4,*)
            read (1, *, iostat=stat, iomsg=msg)              f444
         class default
            error stop 5_4
      end select
   end associate

   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 6_4

   ! check if the values are read correctly

   if ( (.not. precision_r4(f3%r,1.0)) .or. ( f3%i /= 2 ) .or. (f3%c /= 'A' ) )  error stop 7_4

   select type ( f4 )
      type is (child(4,4,*)) ! tcx: (4,4,*)
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
   class(child(4,4,*)), intent(inout) :: dtv ! tcx: (4,4,*)
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
   class(child(4,4,*)), intent(in) :: dtv ! tcx: (4,4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( T1,I4.3,TR1,A1,1X,F5.3 )

   write (unit, 10, iostat=iostat )          dtv%i, dtv%c, dtv%r

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kc,lc) to invoke with (4,4,1) / declare with (4,4,*) - 13 changes
