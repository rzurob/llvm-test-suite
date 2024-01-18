! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : structureComp003allll
!*
!*  DATE                       : 2007-09-19 (original: 11/08/2004)
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

   type container (lcontainer_1,lcontainer_2) ! lcontainer_1,lcontainer_2=3,3
      integer, len :: lcontainer_1,lcontainer_2
      type(child(lcontainer_1,lcontainer_2)) :: b1 ! tcx: (lcontainer_1,lcontainer_2)
      type(child(lcontainer_1,lcontainer_2)) :: b2 ! tcx: (lcontainer_1,lcontainer_2)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp003allll
   use m1

    interface read(unformatted)
        subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base(*)), intent(inout) :: dtv ! tcx: (*)
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

   ! declaration of variables
   class(container(:,:)), allocatable  :: b11(:) ! tcx: (:,:)
   class(container(:,:)), pointer      :: b12(:,:) ! tcx: (:,:)
   type (container(3,3))               :: b13(3) ! tcx: (3,3)
   integer :: stat
   character(200) :: msg
   character(21)  :: c1
   character(16)  :: c2
   character(8)  :: c3
   character(22)  :: c4

   ! allocation of variables
   allocate ( b11(3), source = (/ container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') ),  & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
                                  container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') ),  &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
                                  container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') )   /) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)

   allocate ( b12(2,2), source = reshape ( source = (/ container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') ),      & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
                                                       container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') ),      &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
                                                       container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') ),      &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
                                                       container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') )   /), & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
                                                       shape = (/2,2/) ) )

   b13 = (/ container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') ),  & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
            container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') ),  &  ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
            container(3,3)( b1=child(3,3)('xxx','xxx'), b2=child(3,3)('xxx','xxx') )   /) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)

   open (unit = 1, file ='structureComp003allll.data', form='unformatted', access='stream')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, pos=37 )            'abcdefghiABCDEFGHI'
   write (1, iostat=stat, iomsg=msg, pos=19 )            'ABCDEFGHIJKL'
   write (1, iostat=stat, iomsg=msg, pos=31 )            '123456'
   write (1, iostat=stat, iomsg=msg, pos=1  )            'jklmnopqrPQRstuSTU'

   read (1, iostat=stat, iomsg=msg, pos=37 )             b11%b1%base, b11%b2%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                 error stop 101_4
      if ( ( b11(1)%b1%c /= 'abc' ) .or. ( b11(1)%b1%cc /= 'xxx' )    .or. &
           ( b11(1)%b2%c /= 'ABC' ) .or. ( b11(1)%b2%cc /= 'xxx' )    .or. &
           ( b11(2)%b1%c /= 'def' ) .or. ( b11(2)%b1%cc /= 'xxx' )    .or. &
           ( b11(2)%b2%c /= 'DEF' ) .or. ( b11(2)%b2%cc /= 'xxx' )    .or. &
           ( b11(3)%b1%c /= 'ghi' ) .or. ( b11(3)%b1%cc /= 'xxx' )    .or. &
           ( b11(3)%b2%c /= 'GHI' ) .or. ( b11(3)%b2%cc /= 'xxx' ))   error stop 2_4
      msg = ''

   read (1, iostat=stat, iomsg=msg, pos=19 )             b12%b2%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                error stop 3_4
      if ( ( b12(1,1)%b1%c /= 'xxx' ) .or. ( b12(1,1)%b1%cc /= 'xxx' )    .or. &
           ( b12(1,1)%b2%c /= 'ABC' ) .or. ( b12(1,1)%b2%cc /= 'xxx' )    .or. &
           ( b12(2,1)%b1%c /= 'xxx' ) .or. ( b12(2,1)%b1%cc /= 'xxx' )    .or. &
           ( b12(2,1)%b2%c /= 'DEF' ) .or. ( b12(2,1)%b2%cc /= 'xxx' )    .or. &
           ( b12(1,2)%b1%c /= 'xxx' ) .or. ( b12(1,2)%b1%cc /= 'xxx' )    .or. &
           ( b12(1,2)%b2%c /= 'GHI' ) .or. ( b12(1,2)%b2%cc /= 'xxx' )    .or. &
           ( b12(2,2)%b1%c /= 'xxx' ) .or. ( b12(2,2)%b1%cc /= 'xxx' )    .or. &
           ( b12(2,2)%b2%c /= 'JKL' ) .or. ( b12(2,2)%b2%cc /= 'xxx' ))   error stop 4_4
      msg = ''

   read (1, iostat=stat, iomsg=msg, pos=31 )             b13(1:3:2)%b1%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                 error stop 5_4
      if ( ( b13(1)%b1%c /= '123' ) .or. ( b13(1)%b1%cc /= 'xxx' )    .or. &
           ( b13(1)%b2%c /= 'xxx' ) .or. ( b13(1)%b2%cc /= 'xxx' )    .or. &
           ( b13(2)%b1%c /= 'xxx' ) .or. ( b13(2)%b1%cc /= 'xxx' )    .or. &
           ( b13(2)%b2%c /= 'xxx' ) .or. ( b13(2)%b2%cc /= 'xxx' )    .or. &
           ( b13(3)%b1%c /= '456' ) .or. ( b13(3)%b1%cc /= 'xxx' )    .or. &
           ( b13(3)%b2%c /= 'xxx' ) .or. ( b13(3)%b2%cc /= 'xxx' ))   error stop 6_4
      msg = ''

   read (1, iostat=stat, iomsg=msg, pos=1 )             b11(2:3)%b1%base, b11(1:2)%b2
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                 error stop 7_4
      if ( ( b11(1)%b1%c /= 'abc' ) .or. ( b11(1)%b1%cc /= 'xxx' )    .or. &
           ( b11(1)%b2%c /= 'pqr' ) .or. ( b11(1)%b2%cc /= 'PQR' )    .or. &
           ( b11(2)%b1%c /= 'jkl' ) .or. ( b11(2)%b1%cc /= 'xxx' )    .or. &
           ( b11(2)%b2%c /= 'stu' ) .or. ( b11(2)%b2%cc /= 'STU' )    .or. &
           ( b11(3)%b1%c /= 'mno' ) .or. ( b11(3)%b1%cc /= 'xxx' )    .or. &
           ( b11(3)%b2%c /= 'GHI' ) .or. ( b11(3)%b2%cc /= 'xxx' ))   error stop 8_4
      msg = ''

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 9_4

   select type (dtv)
      type is (child(*,*)) ! tcx: (*,*)
         read (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
   end select

   iomsg = 'basedtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 23 changes
! type: container - added parameters (lcontainer_1,lcontainer_2) to invoke with (3,3) / declare with (*,*) - 13 changes
