!*  ===================================================================
!*
!*                               Language Level Options
!*
!*  DATE                       : April 11, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : -qlanglvl=[extended|2003pure|2003std] Option
!*  SECONDARY FUNCTIONS TESTED : ASYNCH= Specifier in the:  OPEN(), and
!*                               INQUIRE() Statements
!*
!*                               langLvlAsynchSpec01p:  xlf95
!*                               langLvlAsynchSpec01s:  xlf95
!*  REQUIRED COMPILER OPTIONS  : langLvlAsynchSpec01e:  -qlanglvl=extended
!*                               langLvlAsynchSpec01p:  -qlanglvl=2003pure
!*                               langLvlAsynchSpec01s:  -qlanglvl=2003std
!*
!*  KEYWORD(S)                 : OPEN(), INQUIRE(), ASYNCH= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*  Code which uses the IBM Extension(s) noted below.
!*
!*  OPEN
!*
!*    Purpose
!*      The OPEN statement can be used to connect an existing external file
!*      to a unit, create an external file that is preconnected, create an
!*      external file and connect it to a unit, or change certain specifiers
!*      of a connection between an external file and a unit.
!*
!*      Syntax
!*         OPEN ( open_list )
!*
!*         ...
!*
!*         ----------    IBM  Extension    ----------
!*         ASYNCH= char_expr
!*           is an asynchronous I/O specifier that indicates whether an
!*            explicitly connected unit is to be used for asynchronous I/O.
!*         ---------- End of IBM Extension ----------
!*
!*  INQUIRE
!*
!*    Purpose
!*      The INQUIRE statement obtains information about the properties of a
!*      named file or the connection to a particular unit.
!*
!*      Syntax
!*
!*         INQUIRE ( inquiry_list )
!*
!*         ...
!*
!*         ----------    IBM  Extension    ----------
!*         ASYNCH= char_variable
!*           indicates whether the unit is connected for asynchronous access.
!*         ---------- End of IBM Extension ----------
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM langLvlAsynchSpec01

    INTEGER, PARAMETER :: n = 10000

    INTEGER, DIMENSION( n ) :: ioID
    INTEGER, DIMENSION( n ) :: dataValues = (/ (i, i = 1, n) /)

    CHARACTER(LEN = 7) :: isAsynch

    CHARACTER(LEN = 256) :: iMsg


    !
    !  IBM Extension ASYNCH= Specifier used.
    !
    OPEN(214, ASYNCH='yes', RECL=4, FORM='unformatted',&
        ACTION='write', ACCESS='direct', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        error stop 1
    end if


    do i = 1, n
        write(214, ASYNCHRONOUS='yes', REC=i,&
            ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        if (iStat /= 0) then
            write(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            error stop 2
        end if
    end do


    !
    !  IBM Extension ASYNCH= Specifier used.
    !
    INQUIRE(214, ASYNCH=isAsynch, IOSTAT=iStat, IOMSG=iMsg)
    isAsynch = 'YES'
    if (iStat /= 0) then
        write(0, *) "INQUIRE() <", iStat, "> ", iMsg
        error stop 3

    else if (isAsynch /= 'YES') then
        write(0, *) "INQUIRE(ASYNCH=", isAsynch, ")"
        error stop 4

    else
        do i = 1, n
            wait(214, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)

            if (iStat /= 224) then
                write(0, *) i, ") WAIT(", ioID( i ), ") <", iStat, "> ", iMsg
                error stop 5
            end if
        end do
    end if


    CLOSE(214, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        error stop 6
    end if

END PROGRAM langLvlAsynchSpec01
