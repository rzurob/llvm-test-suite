! GB DTP extension using:
! ftcx_dtp /tstdev/F2003/asynchIO/statements/dtio/childAsynchSpec01.f
! opt variations: -qck

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : childAsynchSpec01 - Derived-Type I/O
!*                               (DTIO) Interactions with Asynchronous I/O
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April 12, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Formatted Derived-Type I/O (DTIO) Routines
!*                               Defined for a Derived-Type
!*  SECONDARY FUNCTIONS TESTED : ASYNCHRONOUS= Specifier in the Child I/O
!*                               Statements; ID= Specifier a Child I/O
!*                               Statement
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), ID= Specifier, ASYNCHRONOUS=
!*                               Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*
!*  9.5 Data transfer statements
!*
!*  R910 read-stmt        is  READ ( io-control-spec-list )[ input-item-list]
!*  R911 write-stmt       is  WRITE ( io-control-spec-list )[ output-item-list]
!*
!*
!*  9.5.1 Control information list
!*
!*  R913 io-control-spec  is  [ UNIT = ] io-unit
!*                        or  [ FMT = ] format
!*                        or  [ NML = ] namelist-group-name
!*                        or  ADVANCE = scalar-default-char-expr
!*                        or  ASYNCHRONOUS = scalar-char-initialization-expr
!*  ...
!*                        or  ID = scalar-int-variable
!*
!*
!*  9.5.1.4 ASYNCHRONOUS= specifier in a data transfer statement
!*
!*  The ASYNCHRONOUS= specifier determines whether this input/output
!*  statement is synchronous or asynchronous.
!*
!*
!*  9.5.1.8 ID= specifier in a data transfer statement
!*
!*  Successful execution of an asynchronous data transfer statement
!*  containing an ID= specifier causes the variable specified in the
!*  ID= specifier to become defined with a processor-dependent value.
!*
!*
!*  9.5.3.7.2 User-defined derived-type input/output procedures
!*
!*  Neither a parent nor child data transfer statement shall be asynchronous.
!*
!*  NOTE 9.48
!*  A child data transfer statement shall not specify the ID=, POS=, or
!*  REC= specifiers in an input/output control list.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mUserData

    type tUserData(k1,n1)    ! (4,30)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: sin
        character(n1) :: name
    end type tUserData


    interface read(formatted)
        subroutine readUserData(userObj, unit, iotype, v_list, iostat, iomsg)
            import tUserData
            class(tUserData(4,*)), intent(inout) :: userObj
            integer, intent(in) :: unit
            character(len = *), intent(in) :: iotype
            integer, dimension( : ), intent(in) :: v_list
            integer, intent(out) :: iostat
            character(len = *), intent(inout) :: iomsg
        end subroutine readUserData
    end interface

    interface write(formatted)
        subroutine writeUserData(userObj, unit, iotype, v_list, iostat, iomsg)
            import tUserData
            class(tUserData(4,*)), intent(in) :: userObj
            integer, intent(in) :: unit
            character(len = *), intent(in) :: iotype
            integer, dimension( : ), intent(in) :: v_list
            integer, intent(out) :: iostat
            character(len = *), intent(inout) :: iomsg
        end subroutine writeUserData
    end interface

end module mUserData


program childAsynchSpec01
    use mUserData

    type(tUserData(4,30)) :: userIn
    type(tUserData(4,30)) :: userOut = tUserData(4,30)(123456789,"Exit, Stage-Right")

    character(len = 256) :: iMsg


    open(203, file='childAsynchSpec01.dat',&
        access='stream', asynchronous='yes', iomsg=iMsg,&
        form='formatted', action='readwrite', iostat=iStat)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    !
    !  Dump the Diagnostic Message (if any), but don't terminate.
    !
    write(203, '(DT(40,40))', iomsg=iMsg, iostat=iStat) userOut
    if (iStat /= 0) then
        write(0, *) "WRITE() <", iStat, "> ", iMsg
    end if


    rewind(203, iomsg=iMsg, iostat=iStat)
    if (iStat /= 0) then
        write(0, *) "REWIND() <", iStat, "> ", iMsg
        call zzrc( 2 )
    end if


    !
    !  Dump the Diagnostic Message (if any), but don't terminate.
    !
    read(203, '(DT(40,40))', iomsg=iMsg, iostat=iStat) userIn
    if (iStat /= 0) then
        write(0, *) "READ() <", iStat, "> ", iMsg
    end if


    close(203, iomsg=iMsg, iostat=iStat)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 3 )
    end if

end program childAsynchSpec01


subroutine readUserData(userObj, unit, iotype, v_list, iostat, iomsg)
    use mUserData, only : tUserData

    class(tUserData(4,*)), intent(inout) :: userObj
    integer, intent(in) :: unit
    character(len = *), intent(in) :: iotype
    integer, dimension( : ), intent(in) :: v_list
    integer, intent(out) :: iostat
    character(len = *), intent(inout) :: iomsg


    read(unit, '(I9," ",A30)', iomsg=iomsg,&
        asynchronous='yes', iostat=iostat) userObj%sin, userObj%name

end subroutine readUserData

subroutine writeUserData(userObj, unit, iotype, v_list, iostat, iomsg)
    use mUserData, only : tUserData

    class(tUserData(4,*)), intent(in) :: userObj
    integer, intent(in) :: unit
    character(len = *), intent(in) :: iotype
    integer, dimension( : ), intent(in) :: v_list
    integer, intent(out) :: iostat
    character(len = *), intent(inout) :: iomsg


    write(unit, '(I9," ",A30)', iostat=iostat, iomsg=iomsg,&
        id=ioID, asynchronous='yes') userObj%sin, userObj%name

end subroutine writeUserData
