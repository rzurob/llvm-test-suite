!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComp002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an structure component for array case
!*                               Sequential Access
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
   
   type base
      character(3) :: c = 'xxx' 
      contains
         procedure, pass :: getC
   end type
      
   type container
      type(base) :: b1
      type(base) :: b2
   end type   
 
   
contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c      
   end function       
end module

program structureComp002
   use m1   
   
   interface read(unformatted)
      subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      
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
   type(container)               :: b13(4)
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   allocate ( b11(3), source = (/ container( b2=base('xxx'), b1=base('xxx') ),       & 
                                  container( b2=base('xxx'), b1=base('xxx') ),       &
                                  container( b2=base('xxx'), b1=base('xxx') ) /) )
                                  
   allocate ( b12(2,2), source = reshape ( source = (/ container( b2=base('xxx'), b1=base('xxx') ),    &
                                                       container( b2=base('xxx'), b1=base('xxx') ),    &
                                                       container( b2=base('xxx'), b1=base('xxx') ),    &
                                                       container( b2=base('xxx'), b1=base('xxx') ) /), &
                                                       shape=(/2,2/) ) )   
   
   b13 = (/ container( b2=base('xxx'), b1=base('xxx') ),       & 
            container( b2=base('xxx'), b1=base('xxx') ),       &
            container( b2=base('xxx'), b1=base('xxx') ),       &
            container( b2=base('xxx'), b1=base('xxx') )     /)

  
   
   open (unit = 1, file ='structureComp002.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )             'abcABCdefDEFghiGHI'
   write (1, iostat=stat, iomsg=msg )             'JKLMNOPQRjklmnopqr'
   write (1, iostat=stat, iomsg=msg )             'abcdefghijkl'
   write (1, iostat=stat, iomsg=msg )             'ABCDEF'
   write (1, iostat=stat, iomsg=msg )             'ghiGHIabcABCdefDEFjklJKL'
   write (1, iostat=stat, iomsg=msg )             'ghiabcdefjkl'
   
   rewind 1
   
   read (1, iostat=stat, iomsg=msg )             b11
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )           error stop 1_4
      if ( ( b11(1)%b1%c /= 'abc' ) .or. ( b11(1)%b2%c /= 'ABC' )    .or. &
           ( b11(2)%b1%c /= 'def' ) .or. ( b11(2)%b2%c /= 'DEF' )    .or. &
           ( b11(3)%b1%c /= 'ghi' ) .or. ( b11(3)%b2%c /= 'GHI' ))   error stop 2_4
      msg = ''
      
   read (1, iostat=stat, iomsg=msg )             b11%b1, b11%b2          
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )               error stop 3_4
      if ( ( b11(1)%b1%c /= 'JKL' ) .or. ( b11(1)%b2%c /= 'jkl' )   .or. &
           ( b11(2)%b1%c /= 'MNO' ) .or. ( b11(2)%b2%c /= 'mno' )   .or. &
           ( b11(3)%b1%c /= 'PQR' ) .or. ( b11(3)%b2%c /= 'pqr' ))  error stop 4_4
      msg = ''
         
   read (1, iostat=stat, iomsg=msg )             b12(2:1:-1,1)
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )               error stop 5_4
      if ( ( b12(1,1)%b1%c /= 'ghi' ) .or. ( b12(1,1)%b2%c /= 'jkl' )    .or. &
           ( b12(2,1)%b1%c /= 'abc' ) .or. ( b12(2,1)%b2%c /= 'def' )    .or. &
           ( b12(1,2)%b1%c /= 'xxx' ) .or. ( b12(1,2)%b2%c /= 'xxx' )    .or. &
           ( b12(2,2)%b1%c /= 'xxx' ) .or. ( b12(2,2)%b2%c /= 'xxx' ))   error stop 6_4
      msg = ''
      
   read (1, iostat=stat, iomsg=msg )             b12(2:1:-1,2)%b1
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                    error stop 7_4
      if ( ( b12(1,1)%b1%c /= 'ghi' ) .or. ( b12(1,1)%b2%c /= 'jkl' )    .or. &          !<- no change after this read
           ( b12(2,1)%b1%c /= 'abc' ) .or. ( b12(2,1)%b2%c /= 'def' )    .or. &          !<- no change after this read
           ( b12(1,2)%b1%c /= 'DEF' ) .or. ( b12(1,2)%b2%c /= 'xxx' )    .or. &
           ( b12(2,2)%b1%c /= 'ABC' ) .or. ( b12(2,2)%b2%c /= 'xxx' ))   error stop 8_4
      msg = ''
   
   read (1, iostat=stat, iomsg=msg )             b13((/3,1,2,4/))
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )               error stop 9_4
      if ( ( b13(1)%b1%c /= 'abc' ) .or. ( b13(1)%b2%c /= 'ABC' )        .or. &
           ( b13(2)%b1%c /= 'def' ) .or. ( b13(2)%b2%c /= 'DEF' )        .or. &
           ( b13(3)%b1%c /= 'ghi' ) .or. ( b13(3)%b2%c /= 'GHI' )        .or. &
           ( b13(4)%b1%c /= 'jkl' ) .or. ( b13(4)%b2%c /= 'JKL' ))       error stop 10_4
      msg = ''
      
   read (1, iostat=stat, iomsg=msg )             b13((/3,1,2,4/))%b2
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                    error stop 11_4
      if ( ( b13(1)%b1%c /= 'abc' ) .or. ( b13(1)%b2%c /= 'abc' )        .or. &
           ( b13(2)%b1%c /= 'def' ) .or. ( b13(2)%b2%c /= 'def' )        .or. &
           ( b13(3)%b1%c /= 'ghi' ) .or. ( b13(3)%b2%c /= 'ghi' )        .or. &
           ( b13(4)%b1%c /= 'jkl' ) .or. ( b13(4)%b2%c /= 'jkl' ))       error stop 12_4
      msg = ''
      
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
use m1
   class(container), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   interface read(unformatted)
      subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface    

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%b1, dtv%b2
   
   if ( (iostat /= 0 ) .or. ( iomsg /= 'basedtio' ))       error stop 7_4
   
   iomsg = 'containerdtio'
   
end subroutine

subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c
   iomsg = 'basedtio'
   
end subroutine
