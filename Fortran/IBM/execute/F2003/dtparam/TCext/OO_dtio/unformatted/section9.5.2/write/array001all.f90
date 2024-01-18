! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001all
!*
!*  PROGRAMMER                 : David Forster (derived from array001a by Robert Ma)
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be polymorphic arrays with array section
!*                                 - vector subscripts, elements (with class hierarchy)
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
   end type
   
   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
   end type
   
contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c      
   end function   
 
end module


program array001all
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
   class(base(:)) , allocatable :: b1(:) ! tcx: (:)
   class(child(:,:)) , allocatable :: b2(:,:) ! tcx: (:,:)
   class(base(:)) , pointer     :: b3(:) ! tcx: (:)
   class(child(:,:)), pointer     :: b4(:,:) ! tcx: (:,:)
   integer :: stat
   character(200) :: msg
   character(28)  :: c1
   character(14)  :: c2
   character(7)  :: c3
   character(14) :: c4
   
   ! allocation of variables
   allocate ( b1(4), source = (/ child(3,3)('abc','ABC'), child(3,3)('def','DEF'), child(3,3)('ghi','GHI'), child(3,3)('jkl','JKL') /) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b2(2,2), source = reshape (source = (/ child(3,3)('ABC','abc'), child(3,3)('DEF','def'), child(3,3)('GHI','ghi'), child(3,3)('JKL','jkl')  /), shape=(/2,2/) ) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b3(4), source = (/ child(3,3)('mno', 'MNO'), child(3,3)('pqr', 'PQR'), child(3,3)('stu', 'STU'), child(3,3)('vwx','VWX') /) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b4(2,2), source = reshape (source = (/ child(3,3)('MNO', 'mno'), child(3,3)('PQR','pqr'), child(3,3)('STU', 'stu'), child(3,3)('VWX','vwx')  /), shape=(/2,2/) ) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   
   open (unit = 1, file ='array001all.data', form='unformatted', recl=50, access='direct')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg, rec=4 )             b1((/3,2,4,1/))    !<- writes "ghiGHIZdefDEFZjklJKLZabcABCZ" (try array sectino with vector subscript)
   write (1, iostat=stat, iomsg=msg, rec=3 )             b2(1:2, 1)         !<- writes "ABCabcZDEFdefZ" (try array section with subscript triplet)
   write (1, iostat=stat, iomsg=msg, rec=2 )             b3(3)              !<- writes "stuSTUZ" (try array element)
   write (1, iostat=stat, iomsg=msg, rec=1 )             b4(1:2:2, 1:2:1)   !<- writes "MNOmnoZSTUstuZ" (try array section withe subscript triplet)
   
   read (1, iostat=stat, iomsg=msg, rec=1 )              c4
   read (1, iostat=stat, iomsg=msg, rec=2 )              c3
   read (1, iostat=stat, iomsg=msg, rec=3 )              c2
   read (1, iostat=stat, iomsg=msg, rec=4 )              c1
   
   ! check if the values are set correctly
   if ( c1 /= 'ghiGHIZdefDEFZjklJKLZabcABCZ' )  error stop 101_4
   if ( c2 /= 'ABCabcZDEFdefZ' )                error stop 2_4
   if ( c3 /= 'stuSTUZ' )                       error stop 3_4
   if ( c4 /= 'MNOmnoZSTUstuZ' )                error stop 4_4
   
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

    select type (dtv)
       type is (child(*,*)) ! tcx: (*,*)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc    
    end select
    
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
        
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 5 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 19 changes
