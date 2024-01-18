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
! %GROUP: formattedrw001.f
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
!*  DESCRIPTION                : Testing: unformatted record may be read or written only by
!*                                        unformatted read or write
!*                                           - unformatted records with formatted read/write
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
    type :: base
        character(3) :: i 
    end type
   
end module

program formattedrw001
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class(base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class(base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    
    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in)  :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    
    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in)  :: v_list(:)            
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    
    integer :: stat
    
    class(base), allocatable :: b1
    class(base), pointer     :: b2
    character(3) :: c
             
    open(1, file='formattedrw001.data', access='sequential', form='unformatted')
   
    allocate (b1 , source= base('IBM'))
    allocate (b2 , source= base('FTN'))
    
    write(1, iostat=stat) b1             !<- unformatted records with unformatted write
    if (stat /= 0 ) error stop 1_4
    
    rewind 1
    
    read (1, iostat=stat) b2             !<- unformatted records with unformatted read
    if (stat /= 0 ) error stop 2_4
    
    write(1, *, iostat=stat)     b1      !<- unformatted records with formatted write
    if (stat == 0 ) error stop 3_4
    
    backspace 1
    
    read (1, fmt=*, iostat=stat) b2      !<- unformatted records with formatted read
    if (stat == 0 ) error stop 4_4
    
    ! close the file appropriately
   
    close ( 1, status ='delete' )
               
end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp
 
    read (unit, iostat=iostat, iomsg=iomsg ) temp
    dtv%i = temp
   
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
       
end subroutine

subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:) 
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp

    read (unit, *, iostat=iostat, iomsg=iomsg) temp
    dtv%i = temp
   
end subroutine


subroutine formattedWrite (dtv, unit, iotype, v_list,iostat, iomsg)
use m
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)     
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
      
    write (unit, *, iostat=iostat, iomsg=iomsg) dtv%i

end subroutine