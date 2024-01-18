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
! %GROUP: C936_001.f
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
!*  DATE                       : 11/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: TYPE(derived-type-spec) in DTIO subroutine
!*                                        shall be invoked for non-extensible type
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
    type base1
        sequence
        character(3) :: i
    end type
    
    type, bind(c) :: base2
        integer(4) :: i
    end type
    
end module

program C936_001
    use m

    interface read(unformatted)
    
        subroutine unformattedRead1 (dtv, unit, iostat, iomsg)
        use m
            type(base1), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
        subroutine unformattedRead2 (dtv, unit, iostat, iomsg)
        use m
            type(base2), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
    end interface
    
    interface write(unformatted)
        
        subroutine unformattedWrite1 (dtv, unit, iostat, iomsg)
        use m
            type(base1), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
        subroutine unformattedWrite2 (dtv, unit, iostat, iomsg)
        use m
            type(base2), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
    end interface

    integer :: stat
    character(10) :: errmsg = ""
    
    type(base1), allocatable :: b1, b2
    type(base2), pointer     :: b3, b4
       
    open(1, file='C936_001.data', access='sequential', form='unformatted')
       
    allocate (b1 , source= base1('IBM'))
    allocate (b2 , source= base1('FUN'))
    allocate (b3 , source= base2(999) )
    allocate (b4 , source= base2(111) )
    
    write(1, iostat=stat, iomsg = errmsg) b1
     
    if (stat /= 0)                 error stop 1_4
    if (errmsg /= "")              error stop 2_4
    
    write(1, iostat=stat, iomsg = errmsg) b3
    
    if (stat /= 0)                 error stop 3_4
    if (errmsg /= "")              error stop 4_4
    
    rewind 1
    
    read (1, iostat=stat) b2
    if (stat /= 0)                 error stop 5_4
    
    read (1, iostat=stat) b4
    if (stat /= 0)                 error stop 6_4
    
    if ( b2%i /= 'IBM' )           error stop 7_4
    if ( b4%i /= 999 )             error stop 8_4   
   
    ! close the file appropriately
   
    close ( 1, status ='delete' )
           
end program


subroutine unformattedRead1 (dtv, unit, iostat, iomsg)
use m
    type(base1), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp
 
    read (unit, iostat=iostat, iomsg=iomsg ) temp
        
    dtv%i = temp
 
end subroutine


subroutine unformattedWrite1 (dtv, unit, iostat, iomsg)
use m
    type(base1), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
       
end subroutine

subroutine unformattedRead2 (dtv, unit, iostat, iomsg)
use m
    type(base2), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) :: temp
 
    read (unit, iostat=iostat, iomsg=iomsg ) temp
    dtv%i = temp
   
end subroutine

subroutine unformattedWrite2 (dtv, unit, iostat, iomsg)
use m, only: base2
    type(base2), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
       
end subroutine