## Author: Jc J
## Date: July 26, 2019
##
## Sort functions for the sort program
##

.globl sort.bubble_sort
.globl sort.recursive_merge_sort
.globl sort.iterative_merge_sort

.data

tmp:			.space 8		# space for temp array for merge operations
								# { n_elements, *elements }										

.text

## bubble_sort : void
##
## API function to sort a list using bubble sort.
##
## Arguments:
##		a0:		address of the list to sort in place
##
## Register usage:
##		s0:		address of the list to sort in place
##				scratch space
##
sort.bubble_sort:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		sw			$s0, 24($sp)
		sw			$s1, 20($sp)
		sw			$s2, 16($sp)
		sw			$s3, 12($sp)
		sw			$s4, 8($sp)
		
		sw			$a0, 8($sp)						# local var: list
		
		li			$s2, 1
		sw			$s2, 4($sp)						# local var: swapped = true
		
		li			$s2, 1
		sw			$s2, 0($sp)						# local var: pass
		
		lw			$s0, 0($a0)						# j/sentinel = n_elements
		
		jal			debug.newline
		
	__bs_outer_loop:
	
		lw			$s2, 4($sp)		
		beqz		$s2, __bs_end					# if swapped = false, we are done.
		
		li			$s2, 0
		sw			$s2, 4($sp)						# swapped = false
				
		li			$s1, 1							# s1: i = 1
		
		jal			debug.bubble_1
		
	__bs_inner_loop:
	
		bge			$s1, $s0, __bs_inner_loop_end	# loop until we hit the sentinel

		lw			$s2, 8($sp)
		lw			$s2, 4($s2)						# s2: list.elements
		
		sub			$s3, $s1, 1						# s3: offset (i - 1)
		mul			$s3, $s3, 4						
		
		addu		$s2, $s2, $s3					# s3: &list.elements[i-1]
		lw			$s3, ($s2)						# s3: *list.elements[i-1]
		
		addu		$s2, $s2, 4						# s2: &list.elements[i]
		lw			$s4, ($s2)						# s4: *list.elements[i]
		
		bgt			$s4, $s3, __bs_skip_swap		# if *list.elements[i] > *list.elements[i - 1]
													# don't swap; elements are already in order
	__bs_swap:
	
		jal			debug.bubble_2
		
		sub			$a0, $s1, 1						# 1st arg: i - 1
		move		$a1, $s1						# 2nd arg: i
		jal			debug.bubble_3
	
		sw			$s3, ($s2)						# list.elements[i] = list.elements[i-1]
		subu		$s2, $s2, 4
		sw			$s4, ($s2)						# list.elements[i-1] = list.elements[i]
		
		jal			debug.bubble_4
				
		li			$s2, 1
		sw			$s2, 4($sp)						# swapped = true			
		
	__bs_skip_swap:
	
		addi		$s1, $s1, 1						# i++
		b			__bs_inner_loop
		
	__bs_inner_loop_end:
		
		subi	  	$s0, $s0, 1						# subtract 1 from sentinel
		
		lw			$s2, 0($sp)						# increment pass
		addi		$s2, $s2, 1
		sw			$s2, 0($sp)
		
		b			__bs_outer_loop
				
	__bs_end:			
	
		jal			debug.bubble_5
		
		lw			$ra, 28($sp)
		lw			$s0, 24($sp)
		lw			$s1, 20($sp)
		lw			$s2, 16($sp)
		lw			$s3, 12($sp)
		lw			$s4, 8($sp)
		addu		$sp, $sp, 32
		jr			$ra	
## end of bubble_sort


## recursive_merge_sort : void
##
## API function to sort a list using merge sort recursively.
## Sets up the call to __do_recursive_merge_sort, which actually does the work.
##
## Arguments:
##		a0:		address of the list to sort in place
##
## Register usage:
##		s0:		address of the list to sort in place
##				scratch space
##
sort.recursive_merge_sort:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)		
		sw			$s0, 24($sp)		
		move		$s0, $a0
				
		lw			$a0, 0($s0)
		jal			__initialize_tmp						# initialize the temporary list for merge functions
															# the temporary list has the same length as the unsorted list
															
		jal			debug.newline
		
		move		$a0, $s0								# 1st arg: list to sort
		li			$a1, 0									# 2nd arg: start index
		lw			$a2, 0($s0)								# 3rd arg: end index
		li			$a3, 0									# 4th arg: recursion depth (used for debugging)
		jal			__do_recursive_merge_sort				# sort the list recursively		
		
		jal			debug.newline
		
		lw			$s0, 24($sp)	
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32		
		jr			$ra
## end of recursive_merge_sort


## iterative_merge_sort : void
##
## API function to sort a list using merge sort iteratively.
## Sets up the call to __do_iterative_merge_sort, which actually does the work.
##
## Arguments:
##		a0:		address of the list to sort in place
##
## Register usage:
##		s0:		address of the list to sort in place
##				scratch space
##
sort.iterative_merge_sort:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)		
		sw			$s0, 24($sp)		
		move		$s0, $a0
				
		lw			$a0, 0($s0)
		jal			__initialize_tmp						# initialize the temporary list for merge functions
															# the temporary list has the same length as the unsorted list
		
		move		$a0, $s0								# 1st arg: list to sort
		li			$a1, 0									# 2nd arg: start index
		lw			$a2, 0($s0)								# 3rd arg: end index
		jal			__do_iterative_merge_sort				# sort the list iteratively		
		
		lw			$s0, 24($sp)	
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32		
		jr			$ra
## end of iterative_merge_sort


## __initialize_tmp : void
##
## Initializes the temp array.
## That is, sets it equal to the following list:
## {
##		n_elements,
##		*elements -> [0] * n_elements
## }
##
## Arguments:
##		a0:		size of the temp array
##
## Register usage:
##		s0:		address of the temp array	
##
__initialize_tmp:

		subu		$sp, $sp, 32		
		sw			$s0, 28($sp)
		
		la			$s0, tmp								# s0: address of tmp
		sw			$a0, 0($s0)								# set tmp.n_elements		
		
		mul			$a0, $a0, 4								# allocate n_elements * 4 bytes on the heap
		li			$v0, 9									# and point tmp.elements to it
		syscall
		sw			$v0, 4($s0)
		
		lw			$s0, 28($sp)		
		addu		$sp, $sp, 32		
		jr			$ra
## end of __initialize_tmp


## __do_recursive_merge_sort : void
##
## Sorts a list using merge sort recursively.
##
## Arguments:
##		a0:		address of the list to sort in place
##		a1:		start index
##		a2:		end index
##		a3:		recursion depth (used for debug formatting)
##
## Register usage:
##		s0:		start index
##		s1:		end index
##		s2:		scratch space
##
__do_recursive_merge_sort:

		subu		$sp, $sp, 64
		sw			$ra, 60($sp)
		
		sw			$s0, 56($sp)
		sw			$s1, 52($sp)
		sw			$s2, 48($sp)
		
		sw			$a0, 24($sp)							# local var: list address
		sw			$a1, 20($sp)							# local var: start
		sw			$a2, 16($sp)							# local var: end
		sw			$a3, 12($sp)							# local var: recursion depth
		
		jal			debug.recursive_1
		
		lw			$s0, 20($sp)							# s0: start
		lw			$s1, 16($sp) 							# s1: end
		
		sub			$s2, $s1, $s0							# s2: end - start
		beq			$s2, 1, __do_recursive_merge_sort_end	# return if length of slice is 1
		
		add			$s2, $s1, $s0							# middle = (start + end) / 2
		srl			$s2, $s2, 1
		sw			$s2, 8($sp)								# local var: middle
		
		jal			debug.recursive_2
		
		lw			$a0, 24($sp)							# 1st arg: list address
		lw			$a1, 20($sp)							# 2nd arg: start
		lw			$a2, 8($sp)								# 3rd arg: middle
		
		lw			$a3, 12($sp)							# 4th arg: depth
		add			$a3, $a3, 1								# depth++		
		jal			__do_recursive_merge_sort
		
		lw			$a0, 24($sp)							# 1st arg: list address
		lw			$a1, 8($sp)								# 2nd arg: middle
		lw			$a2, 16($sp)							# 3rd arg: end
		
		lw			$a3, 12($sp)							# 4th arg: depth
		add			$a3, $a3, 1								# depth++		
		jal			__do_recursive_merge_sort
		
		jal			debug.recursive_3
		
		lw			$a0, 24($sp)							# 1st arg: list address
		lw			$a1, 20($sp)							# 2nd arg: start
		lw			$a2, 8($sp)								# 3rd arg: middle
		lw			$a3, 16($sp)							# 4th arg: end
		jal			__merge

		jal			debug.recursive_4
		
	__do_recursive_merge_sort_end:
    
    	lw			$s0, 56($sp)
		lw			$s1, 52($sp)
		lw			$s2, 48($sp)
		
		lw			$ra, 60($sp)
		addu		$sp, $sp, 64
		jr			$ra
## end of __do_recursive_merge_sort

	
## __do_iterative_merge_sort : void
##
## Sorts a list using merge sort iteratively.
##
## Arguments:
##		a0:		address of the list to sort in place
##		a1:		start index
##		a2:		end index
##
## Register usage:
##		s0:		start index
##		s1:		end index
##		s2:		scratch space
##
__do_iterative_merge_sort:

		subu		$sp, $sp, 64
		sw			$ra, 60($sp)
		
		sw			$s0, 56($sp)
		sw			$s1, 52($sp)
		
		sw			$a0, 32($sp)							# local var: list address
		sw			$a1, 28($sp)							# local var: start
		sw			$a2, 24($sp)							# local var: end
		
		li			$s0, 1									# s0: size
		lw			$s1, 0($a0)								# s1: n_elements
		
		jal			debug.newline
		
	__dis_outer_loop:
	
		bge			$s0, $s1, __do_iterative_merge_sort_end
		
		move		$a0, $s0
		jal			debug.iterative_1
		
		li			$s2, 0									# i = 0
		
	__dis_inner_loop:
	
		bge			$s2, $s1, __dis_inner_loop_end
		
		move		$s3, $s2								# start = i
		sw			$s3, 28($sp)
		
		move		$s3, $s2
		add			$s3, $s3, $s0							# middle = i + size
		blt			$s3, $s1, __dis_middle_smaller			# if middle >= unsorted.length
		
	__dis_middle_larger:
	
		move		$s3, $s1								# middle = unsorted.length
		
	__dis_middle_smaller:									# else middle = i + size
	
		sw			$s3, 20($sp)							# middle = min(i+size, unsorted.length)
		
		mul			$s3, $s0, 2
		add			$s3, $s3, $s2							# end = i + 2 * size
		blt			$s3, $s1, __dis_end_smaller				# if end >= unsorted.length
		
	__dis_end_larger:
	
		move		$s3, $s1								# end = unsorted.length
		
	__dis_end_smaller:										# else end = i + 2 * size
	
		sw			$s3, 24($sp)							# end = min(i+2*size, unsorted.length)
	
		
		jal			debug.iterative_2
		
		lw			$a0, 32($sp)							# 1st arg: list address
		lw			$a1, 28($sp)							# 2nd arg: start
		lw			$a2, 20($sp)							# 3rd arg: middle
		lw			$a3, 24($sp)							# 4th arg: end
		jal			__merge
		
		jal			debug.iterative_3	
		
		mul			$s3, $s0, 2								
		add			$s2, $s2, $s3							# i = i + 2 * width
		b			__dis_inner_loop
		
	__dis_inner_loop_end:
	
		mul			$s0, $s0, 2								# width = width * 2
		b __dis_outer_loop
		
	__do_iterative_merge_sort_end:
		
		lw			$s0, 56($sp)
		lw			$s1, 52($sp)
		
		lw			$ra, 60($sp)
		addu		$sp, $sp, 64			
		jr 			$ra
## end of __do_iterative_merge_sort


## __merge : void
##
## Merges two *sorted* lists together.
## The two lists are defined as:
##		left:  list[start:middle]
##		right: list[middle:end]
##
## Arguments:
##		a0:		address of the list being sorted
##		a1:		start index
##		a2:		middle index
##		a3:		end index
##
## Register usage:
##		s0:		i (left index)
##		s1:		j (right index)
##		s2:		k (tmp index)
##		s3:		scratch space
##		s4:		list.elements[i], *list.elements[i]
##		s5:		list.elements[j], *list.elements[j]
##		a1:		tmp.elements[k]
__merge:

		subu    	$sp, $sp, 64
		sw			$ra, 60($sp)
		
		sw			$s0, 56($sp)
		sw			$s1, 52($sp)
		sw			$s2, 48($sp)
		sw			$s3, 44($sp)
		sw			$s4, 40($sp)
		sw			$s5, 36($sp)
		
		sw			$a0, 24($sp)							# local var: list address
		sw			$a1, 20($sp)							# local var: start
		sw			$a2, 16($sp)							# local var: middle
		sw			$a3, 12($sp)							# local var: end
		
		move		$s0, $a1								# i = start (left index)
		move		$s1, $a2								# j = middle (right index)
		move		$s2, $s0								# k = start (tmp index)
		
    m_loop:
    
    	lw			$s3, 24($sp)							# s3: list.elements
    	lw			$s3, 4($s3)
    	
    	mul			$s4, $s0, 4								# s4: list.elements[i]
    	addu		$s4, $s4, $s3							
    	
    	mul			$s5, $s1, 4								# s5: list.elements[j] 
    	addu		$s5, $s5, $s3							   	
  
		la			$a1, tmp								# a1: tmp.elements[k]
		lw			$a1, 4($a1)
		mul			$s3, $s2, 4
		addu		$a1, $a1, $s3									
		    
        lw      	$s3, 16($sp)                            # s3: middle
        beq     	$s0, $s3, m_copy_b_to_a             	# if list a is exhausted, copy rest of list b to a
            
        lw      	$s3, 12($sp)                            # s3: end
        beq     	$s1, $s3, m_copy_a_to_b             	# if list b is exhaused, copy rest of list a to b
        
        lw			$s4, ($s4)								# s4: *list.elements[i]
        lw			$s5, ($s5)								# s5: *list.elements[j]
        
        bgt     	$s4, $s5, m_list_b_smaller 
        
    m_list_a_smaller:
            
        sw      	$s4, ($a1)                              # tmp.elements[k] = list.elements[i]
        add     	$s0, $s0, 1                             # i++
        add     	$s2, $s2, 1                             # k++
        b      		m_loop

    m_list_b_smaller:

        sw      	$s5, ($a1)                              # tmp.elements[k] = list.elements[j]
        add     	$s1, $s1, 1                             # j++
        add     	$s2, $s2, 1                             # k++
        b      		m_loop
        
    m_copy_a_to_b:
    
    	move		$a0, $s4								# 1st arg: list.elements[i]
    														# 2nd arg: tmp.elements[k]    													
    	lw			$s3, 16($sp)							# 3rd arg: n_bytes (middle - start) * 4
    	sub			$a2, $s3, $s0
    	mul			$a2, $a2, 4
		jal			util.memcpy								# memcpy(list[j], tmp[k], n_bytes)		
		b			m_loop_end  
		
	m_copy_b_to_a:
    
    	move		$a0, $s5								# 1st arg: list.elements[j]
    														# 2nd arg: tmp.elements[k]    													
    	lw			$s3, 12($sp)							# 3rd arg: n_bytes (end - middle) * 4
    	sub			$a2, $s3, $s1
    	mul			$a2, $a2, 4
		jal			util.memcpy								# memcpy(list[j], tmp[k], n_bytes)		
		b			m_loop_end
    	
	m_loop_end:												# done; copy tmp[start:end] to list[start:end]

       	lw      	$s0, 20($sp)                            # s0: start
        lw      	$s1, 12($sp)                            # s1: end
        
        mul			$s3, $s0, 4								# s3: offset i
        
        la			$a0, tmp								# 1st arg: tmp.elements[i]
        lw			$a0, 4($a0)
        addu		$a0, $a0, $s3

        lw			$a1, 24($sp)							# 2nd arg: list.elements[i]
        lw			$a1, 4($a1)
        addu		$a1, $a1, $s3
        
        sub			$a2, $s1, $s0							# 3rd arg: n_bytes 
        mul			$a2, $a2, 4								# = (end - start) * 4
        
        jal			util.memcpy          					# memcpy(tmp, list, n_bytes)
			
		lw			$s0, 56($sp)
		lw			$s1, 52($sp)
		lw			$s2, 48($sp)
		lw			$s3, 44($sp)
		lw			$s4, 40($sp)
		lw			$s5, 36($sp)

        lw      	$ra, 60($sp)
        addu    	$sp, $sp, 64 
        jr      	$ra
