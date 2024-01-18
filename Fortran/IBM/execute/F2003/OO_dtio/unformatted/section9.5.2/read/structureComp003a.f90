!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComp003a.f
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

   type base
      character(3) :: c = ''
      contains
         procedure, pass :: getC
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

   type container
      type(child) :: b1
      type(child) :: b2
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp003a
   use m1

    interface read(unformatted)
        subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base), intent(inout) :: dtv
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

   ! declaration of variables
   class(container), allocatable  :: b11(:)
   class(container), pointer      :: b12(:,:)
   type (container)               :: b13(3)
   integer :: stat
   character(200) :: msg
   character(21)  :: c1
   character(16)  :: c2
   character(8)  :: c3
   character(22)  :: c4

   ! allocation of variables
   allocate ( b11(3), source = (/ container( b1=child('xxx','xxx'), b2=child('xxx','xxx') ),  &
                                  container( b1=child('xxx','xxx'), b2=child('xxx','xxx') ),  &
                                  container( b1=child('xxx','xxx'), b2=child('xxx','xxx') )   /) )

   allocate ( b12(2,2), source = reshape ( source = (/ container( b1=child('xxx','xxx'), b2=child('xxx','xxx') ),      &
                                                       container( b1=child('xxx','xxx'), b2=child('xxx','xxx') ),      &
                                                       container( b1=child('xxx','xxx'), b2=child('xxx','xxx') ),      &
                                                       container( b1=child('xxx','xxx'), b2=child('xxx','xxx') )   /), &
                                                       shape = (/2,2/) ) )

   b13 = (/ container( b1=child('xxx','xxx'), b2=child('xxx','xxx') ),  &
            container( b1=child('xxx','xxx'), b2=child('xxx','xxx') ),  &
            container( b1=child('xxx','xxx'), b2=child('xxx','xxx') )   /)

   open (unit = 1, file ='structureComp003a.data', form='unformatted', access='stream')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, pos=37 )            'abcdefghiABCDEFGHI'
   write (1, iostat=stat, iomsg=msg, pos=19 )            'ABCDEFGHIJKL'
   write (1, iostat=stat, iomsg=msg, pos=31 )            '123456'
   write (1, iostat=stat, iomsg=msg, pos=1  )            'jklmnopqrPQRstuSTU'

   read (1, iostat=stat, iomsg=msg, pos=37 )             b11%b1%base, b11%b2%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                 error stop 1_4
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
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 9_4

   select type (dtv)
      type is (child)
         read (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
   end select

   iomsg = 'basedtio'

end subroutine
