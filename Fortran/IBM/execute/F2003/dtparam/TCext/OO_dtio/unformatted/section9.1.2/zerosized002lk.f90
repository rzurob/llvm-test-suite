! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : zerosized002lk
!*
!*  DATE                       : 2007-09-10 (original: 11/04/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Ensure zero-sized type will invoke DTIO for array(sequential access write)
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
    type :: base (lbase_1) ! lbase_1=0
       integer, len :: lbase_1
       character(lbase_1) :: i
    end type

    type :: emptybase (keb) ! keb=1
       integer, kind :: keb
    end type

    type :: nonemptybase (knonemptybase_1) ! knonemptybase_1=1
       integer, kind :: knonemptybase_1
       character(knonemptybase_1) :: c = 'c'
    end type

end module

program zerosized002lk
use m

   interface write(unformatted)
      subroutine unformattedWrite (dtv, unit, iostat, iomsg)
      use m
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

      subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
      use m
         class(emptybase(1)), intent(in) :: dtv ! tcx: (1)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

      subroutine unformattedWriteNonEmpty (dtv, unit, iostat, iomsg)
      use m
         class(nonemptybase(1)), intent(in) :: dtv ! tcx: (1)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

   end interface

   ! declaration of variables

   integer :: stat

   character(3) :: c1
   character(9) :: c2
   character(2) :: c3, c6
   character(4) :: c4
   character(5) :: c5
   character(0) :: c7, c8, c9

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:,:) ! tcx: (:)
   type (base(0))              :: b3(2) ! tcx: (0)

   class(emptybase(1)), allocatable :: e1(:,:) ! tcx: (1)
   class(emptybase(1)), pointer     :: e2(:) ! tcx: (1)
   type (emptybase(1))              :: e3(2) ! tcx: (1)

   class(nonemptybase(1)), allocatable :: n1(:) ! tcx: (1)
   class(nonemptybase(1)), pointer     :: n2(:,:) ! tcx: (1)
   type (nonemptybase(1))              :: n3(0) ! tcx: (1)

   ! allocation of variables

   allocate (base(0):: b1(3), b2(3,3) ) ! tcx: base(0)
   allocate ( e1(2,2), e2(5) )
   allocate (nonemptybase(1):: n1(0), n2(0,0) ) ! tcx: nonemptybase(1)

   open(1, file='zerosized002lk.data', access='sequential', form='unformatted')

   write (1, iostat = stat) b1     !<- shall write "ZZZ"
   write (1, iostat = stat) b2     !<- shall write "ZZZZZZZZZ"
   write (1, iostat = stat) b3     !<- shall write "ZZ"

   write (1, iostat = stat) e1     !<- shall write "XXXX"
   write (1, iostat = stat) e2     !<- shall write "XXXXX"
   write (1, iostat = stat) e3     !<- shall write "XX"

   write (1, iostat = stat) n1     !<- shall write "" (shall not call DTIO since no effective items)
   if ( stat /= 0 )   error stop 101_4
   write (1, iostat = stat) n2     !<- shall write "" (shall not call DTIO since no effective items)
   if ( stat /= 0 )   error stop 2_4
   write (1, iostat = stat) n3     !<- shall write "" (shall not call DTIO since no effective items)
   if ( stat /= 0 )   error stop 3_4

   rewind 1

   read (1, iostat = stat)  c1
   read (1, iostat = stat)  c2
   read (1, iostat = stat)  c3

   read (1, iostat = stat)  c4
   read (1, iostat = stat)  c5
   read (1, iostat = stat)  c6

   read (1, iostat = stat)  c7
   read (1, iostat = stat)  c8
   read (1, iostat = stat)  c9

   ! check if the written values are correct

   if ( c1 /= "ZZZ" )         error stop 4_4
   if ( c2 /= "ZZZZZZZZZ" )   error stop 5_4
   if ( c3 /= "ZZ" )          error stop 6_4

   if ( c4 /= "XXXX" )        error stop 7_4
   if ( c5 /= "XXXXX" )       error stop 8_4
   if ( c6 /= "XX" )          error stop 9_4

   if ( c7 /= "" )            error stop 10_4
   if ( c8 /= "" )            error stop 11_4
   if ( c9 /= "" )            error stop 12_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   ! add a mark at the end of record, so we know DTIO is used
   write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
   write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase(1)), intent(in) :: dtv ! tcx: (1)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   ! add a mark at the end of record, so we know DTIO is used
   write (unit, iostat=iostat, iomsg=iomsg ) "X"

end subroutine

subroutine unformattedWriteNonEmpty (dtv, unit, iostat, iomsg)
use m
   class(nonemptybase(1)), intent(in) :: dtv ! tcx: (1)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iomsg=iomsg ) dtv%c

   iostat = 997

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (0) / declare with (*) - 5 changes
! type: emptybase - added parameters (keb) to invoke with (1) / declare with (1) - 5 changes
! type: nonemptybase - added parameters (knonemptybase_1) to invoke with (1) / declare with (1) - 5 changes
