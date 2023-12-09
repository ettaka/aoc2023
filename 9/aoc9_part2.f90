program aoc9_part2
  implicit none

  integer, parameter :: line_len = 1000
  integer :: stat, prediction_sum

  open(10, file='input.txt', status='old', iostat=stat)

  block
    integer :: i, num, num_start, prediction
    integer, allocatable :: nums(:)
    character(line_len) :: line

    prediction_sum = 0
    do
      if (allocated(nums)) deallocate(nums)
      allocate(nums(0))
      read(10, '(A)', end=99) line
      print *, trim(line)

      num_start=1
      do i = 1, len_trim(line)+1
        if (line(i:i) == ' ') then
          read(line(num_start:i-1), '(I10)') num
          num_start=i
          nums = [nums, [num]]
        end if
      end do
      prediction = extrapolate(nums)
      prediction_sum = prediction_sum + prediction
      print *, prediction
    end do
  end block

  99 continue

  print *, prediction_sum

  contains

    recursive function extrapolate(nums) result (res)
      integer, intent(in) :: nums(:)
      integer :: n, res, prediction
      integer :: diff(size(nums, dim=1)-1)

      n = size(nums, dim=1)
      diff(1:n-1) = nums(2:n)-nums(1:n-1)
      if (all(nums==0)) then
        res=0
      else
        prediction = extrapolate(diff)
        res=nums(1)-prediction
      end if
    end function
end program aoc9_part2
