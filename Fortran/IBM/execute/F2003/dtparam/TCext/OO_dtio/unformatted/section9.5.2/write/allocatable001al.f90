! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : allocatable001al
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an allocatable array
!*                               Direct Access
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
   type base (lbase_1) ! lbase_1=2
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(2) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(2), intent(in) :: char
      a%c = char
   end subroutine
end module

program allocatable001al
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base(:)), allocatable :: b1(:), b2 ! tcx: (:)
   class(base(:)), allocatable :: b3(:,:) ! tcx: (:)
   type(base(:)),  allocatable :: b4(:) ! tcx: (:)
   integer :: stat
   character(200) :: msg
   character(6)   :: c1
   character(9)   :: c2
   character(21)  :: c3
   character(24)  :: c4

   ! allocation of variables
   allocate ( b1(2), source = (/ base(2)('ab'), base(2)('cd') /)  )  !<- 1 dimensional array ! tcx: (2) ! tcx: (2)
   allocate ( b2, source = base(2)('ef') )  !<- scalar ! tcx: (2)
   allocate ( b3(2,2), source = reshape (source=(/base(2)('gh'),base(2)('ij'),base(2)('kl'),base(2)('mn') /), shape=(/2,2/)) )   !<- multi-dimensional array ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)
   allocate ( b4(0), source = (/ (base(2)('xx'),i=1,0)/) )             !<- zero sized array ! tcx: (2)


   open (unit = 1, file ='allocatable001al.data', form='unformatted', access='direct', recl=50)

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, rec=4 )             b1                        !<- write 'abZcdZ' to file
   write (1, iostat=stat, iomsg=msg, rec=3 )             b1, b2                    !<- write 'abZcdZefZ' to file
   write (1, iostat=stat, iomsg=msg, rec=2 )             b1, b2, b3                !<- write 'abZcdZefZghZijZklZmnZ' to file
   write (1, iostat=stat, iomsg=msg, rec=1 )             b1, b2, b3, b4, 'eor'     !<- write 'abZcdZefZghZijZklZmnZeor' to file

   read (1, iostat=stat, iomsg=msg, rec=1 )              c4
   read (1, iostat=stat, iomsg=msg, rec=2 )              c3
   read (1, iostat=stat, iomsg=msg, rec=3 )              c2
   read (1, iostat=stat, iomsg=msg, rec=4 )              c1

   ! check if the values are set correctly
   if ( c1 /= 'abZcdZ' )                          error stop 101_4
   if ( c2 /= 'abZcdZefZ' )                       error stop 2_4
   if ( c3 /= 'abZcdZefZghZijZklZmnZ' )           error stop 3_4
   if ( c4 /= 'abZcdZefZghZijZklZmnZeor' )        error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (2) / declare with (*) - 15 changes
