! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be array, structure component, try parent component
!*                               Stream Access
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

   type container (lcontainer_1) ! lcontainer_1=3
      integer, len :: lcontainer_1
      type(child(lcontainer_1,lcontainer_1)) :: b1 ! tcx: (lcontainer_1,lcontainer_1)
      type(child(lcontainer_1,lcontainer_1)) :: b2 ! tcx: (lcontainer_1,lcontainer_1)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp003alll
   use m1

   interface write(unformatted)
      subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(container(:)), allocatable  :: b11(:) ! tcx: (:)
   class(container(:)), pointer      :: b12(:,:) ! tcx: (:)
   type (container(3))               :: b13(3) ! tcx: (3)
   integer :: stat
   character(200) :: msg
   character(21)  :: c1
   character(16)  :: c2
   character(8)  :: c3
   character(22)  :: c4

   ! allocation of variables
   allocate ( b11(3), source = (/ container(3)( b1=child(3,3)('abc','ABC'), b2=child(3,3)('def','DEF') ),  & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
                                  container(3)( b1=child(3,3)('ghi','GHI'), b2=child(3,3)('jkl','JKL') ),  &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
                                  container(3)( b1=child(3,3)('mno','MNO'), b2=child(3,3)('pqr','PQR') )   /) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)

   allocate ( b12(2,2), source = reshape ( source = (/ container(3)( b1=child(3,3)('abc','ABC'), b2=child(3,3)('def','DEF') ),      & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
                                                       container(3)( b1=child(3,3)('ghi','GHI'), b2=child(3,3)('jkl','JKL') ),      &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
                                                       container(3)( b1=child(3,3)('mno','MNO'), b2=child(3,3)('pqr','PQR') ),      &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
                                                       container(3)( b1=child(3,3)('stu','STU'), b2=child(3,3)('vwx','VWX') )   /), & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
                                                       shape = (/2,2/) ) )

   b13 = (/ container(3)( b1=child(3,3)('abc','ABC'), b2=child(3,3)('def','DEF') ),  & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
            container(3)( b1=child(3,3)('ghi','GHI'), b2=child(3,3)('jkl','JKL') ),  &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
            container(3)( b1=child(3,3)('mno','MNO'), b2=child(3,3)('pqr','PQR') )   /) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)

   open (unit = 1, file ='structureComp003alll.data', form='unformatted', access='stream')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, pos=63 )             b11%b1              !<- write 'abcABCZghiGHIZmnoMNOZ' to file
   write (1, iostat=stat, iomsg=msg, pos=31 )             b12%b2%base         !<- write 'defZjklZpqrZvwxZ' to file
   write (1, iostat=stat, iomsg=msg, pos=23 )             b13(1:3:2)%b1%base  !<- write 'abcZmnoZ' to file
   write (1, iostat=stat, iomsg=msg, pos=1 )              b11(2:3)%b1%base, b11(1:2)%b2 !<- write 'ghiZmnoZdefDEFZjklJKLZ'

   read (1, iostat=stat, iomsg=msg, pos=63 )              c1
   read (1, iostat=stat, iomsg=msg, pos=31 )              c2
   read (1, iostat=stat, iomsg=msg, pos=23 )              c3
   read (1, iostat=stat, iomsg=msg, pos=1  )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abcABCZghiGHIZmnoMNOZ' )   error stop 101_4
   if ( c2 /= 'defZjklZpqrZvwxZ' )        error stop 2_4
   if ( c3 /= 'abcZmnoZ' )                error stop 3_4
   if ( c4 /= 'ghiZmnoZdefDEFZjklJKLZ' )  error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 23 changes
! type: container - added parameters (lcontainer_1) to invoke with (3) / declare with (*) - 13 changes