! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/asynchIO/statements/dtio/bothAsynchSpec01.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*
!*                               (DTIO) Interactions with Asynchronous I/O
!*
!*  DATE                       : April 12, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted Derived-Type I/O (DTIO) Routines
!*                               Defined for a Derived-Type
!*  SECONDARY FUNCTIONS TESTED : ASYNCHRONOUS= Specifier appears in both the
!*                               Parent and Child I/O Statements; ID= Specifier
!*                               appears in a Parent and a Child I/O Statement
!*
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
!*  9.5.1.4 ASYNCHRONOUS= specifier in a data transfer statement
!*
!*  The ASYNCHRONOUS= specifier determines whether this input/output
!*  statement is synchronous or asynchronous.
!*
!*  9.5.1.8 ID= specifier in a data transfer statement
!*
!*  Successful execution of an asynchronous data transfer statement
!*  containing an ID= specifier causes the variable specified in the
!*  ID= specifier to become defined with a processor-dependent value.
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

MODULE mBothAsynchSpec01
    type tBothAsynchSpec01(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: a
        integer(k1)   :: b
    end type tBothAsynchSpec01


    INTERFACE write(unformatted)
        SUBROUTINE bothAsynchSpecWrite(obj, unit, iostat, iomsg)
            import tBothAsynchSpec01
            class(tBothAsynchSpec01(*,4)), intent(in) :: obj
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(len = *), intent(inout) :: iomsg
        END SUBROUTINE bothAsynchSpecWrite
    END INTERFACE

    INTERFACE read(unformatted)
        SUBROUTINE bothAsynchSpecRead(obj, unit, iostat, iomsg)
            import tBothAsynchSpec01
            class(tBothAsynchSpec01(*,4)), intent(inout) :: obj
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(len = *), intent(inout) :: iomsg
        END SUBROUTINE bothAsynchSpecRead
    END INTERFACE

END MODULE mBothAsynchSpec01


SUBROUTINE bothAsynchSpecWrite(obj, unit, iostat, iomsg)
    use mBothAsynchSpec01, only : tBothAsynchSpec01

    class(tBothAsynchSpec01(*,4)), intent(in) :: obj
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(len = *), intent(inout) :: iomsg


    write(unit, asynchronous='yes', id=id,&
        iostat=iostat, iomsg=iomsg) obj%b, obj%a

END SUBROUTINE bothAsynchSpecWrite


SUBROUTINE bothAsynchSpecRead(obj, unit, iostat, iomsg)
    use mBothAsynchSpec01, only : tBothAsynchSpec01

    class(tBothAsynchSpec01(*,4)), intent(inout) :: obj
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(len = *), intent(inout) :: iomsg


    read(unit, asynchronous='yes',&
        iostat=iostat, iomsg=iomsg) obj%b, obj%a

END SUBROUTINE bothAsynchSpecRead


PROGRAM bothAsynchSpec01
    use mBothAsynchSpec01

    type(tBothAsynchSpec01(20,4)) :: bothIn
    type(tBothAsynchSpec01(20,4)) :: bothOut = tBothAsynchSpec01(20,4)(1234,5678)

    character(len = 256) :: iMsg


    open(948, access='stream', action='readwrite', iomsg=iMsg,&
            asynchronous='yes', form='unformatted', iostat=iStat)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    write(948, asynchronous='yes', id=ioID, iomsg=iMsg, iostat=iStat) bothOut
    if (iStat /= 0) then
        write(0, *) "WRITE() <", iStat, "> ", iMsg
    end if


    wait(948, id=ioID, iomsg=iMsg, iostat=iStat)
    if (iStat /= 0) then
        write(0, *) "WAIT(ID=", ioID, ") <", iStat, "> ", iMsg
    end if


    rewind(948, iomsg=iMsg, iostat=iStat)
    if (iStat /= 0) then
        write(0, *) "REWIND() <", iStat, "> ", iMsg
        call zzrc( 2 )
    end if


    read(948, asynchronous='yes', iomsg=iMsg, iostat=iStat) bothIn
    if (iStat /= 0) then
        write(0, *) "READ() <", iStat, "> ", iMsg
    end if


    close(948, iomsg=iMsg, iostat=iStat)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 3 )
    end if

END PROGRAM bothAsynchSpec01
