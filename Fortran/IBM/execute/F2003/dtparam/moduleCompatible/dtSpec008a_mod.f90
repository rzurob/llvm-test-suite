
module m
    type base (n)
        integer, len :: n

        real :: data (n)

        contains

        generic :: add => addData, appendData
        procedure :: addData => addBaseData
        procedure :: appendData => appendBaseData
    end type

    contains

    function addBaseData (b1, b2)
        class(base(*)), intent(in) :: b1, b2
        class(base(n=max(b1%n, b2%n))), allocatable :: addBaseData

        allocate (addBaseData)

        if (b1%n >= b2%n) then
            addBaseData%data = b1%data
            addBaseData%data(:b2%n) = addBaseData%data(:b2%n) + b2%data
        else
            addBaseData%data = b2%data
            addBaseData%data(:b1%n) = addBaseData%data(:b1%n) + b1%data
        end if
    end function

    function appendBaseData (b1, b2, limit)
        class(base(*)), intent(in) :: b1, b2
        integer, intent(in) :: limit
        class(base(n=min(b1%n+b2%n,limit))), allocatable :: appendBaseData

        allocate(appendBaseData)

        if (b1%n + b2%n > limit) then
            appendBaseData%data = reshape((/b1%data, b2%data/), (/limit/))
        else
            appendBaseData%data = (/b1%data, b2%data/)
        end if
    end function
end module
