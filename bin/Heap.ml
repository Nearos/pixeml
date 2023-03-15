exception EmptyOptions

module type Compare = sig 
    type t
    val compare : t -> t -> int
end

module Make (CompareElements : Compare) 
    : sig
        type value_type = CompareElements.t
        type heap_index
        type t

        val empty : unit -> t

        val insert : value_type -> t -> unit
        val pop : t -> value_type option
        val top : t -> value_type option

        val find : (value_type -> bool) -> t -> heap_index

        val remove : heap_index -> t -> unit
    end
    = struct
        module Array = Batteries.Vect

        type value_type = CompareElements.t

        type heap_index = int

        type t = value_type Array.t ref

        let index_parent n = (n + 1) / 2 - 1
        let index_left n = (n + 1) * 2 - 1
        let index_right n = (n + 1) * 2

        let swap (a : int) (b : int) (heap : t) : unit =
            let x = (!heap).(a) in 
            let neep = (!heap).(a) <- (!heap).(b) in
            let neep = (neep).(b) <- x in 
            heap := neep

        let empty () = ref Array.empty

        let min_index_by_heap (indices : int list) (heap : t) =
            let fold_fun acc el = 
                try 
                    if CompareElements.compare (!heap).(acc) (!heap).(el) < 0 
                    then acc
                    else el
                with Array.Out_of_bounds -> acc
            in
            match indices with
            | [] -> raise EmptyOptions
            | x :: xs -> List.fold_left fold_fun x xs 

        let rec upheap (idx : int) (heap : t) : unit = 
            let parent_index = index_parent idx in 
            if min_index_by_heap [idx; parent_index] heap == idx && parent_index >= 0
            then 
                let () = swap idx parent_index heap in
                upheap parent_index heap
            else ()

        let rec downheap (idx : int) (heap : t) : unit = 
            let min = min_index_by_heap [idx; index_left idx; index_right idx] heap in 
            if min == idx 
            then ()
            else 
                let () = swap min idx heap in 
                downheap min heap


        let insert (value : value_type) (heap : t) : unit = 
            heap := Array.append value (!heap);
            let len = Array.length (!heap) in
            upheap (len - 1) heap

        let top (heap : t) : value_type option = 
            if Array.is_empty (!heap)
            then None 
            else Some((!heap).(0))

        let pop (heap : t) : value_type option = 
            match top heap with 
            | None -> None 
            | Some(v) -> 
                let len = Array.length (!heap) in
                let el = (!heap).(len - 1) in
                heap := Array.remove (len - 1) 1 (!heap);
                if not (Array.is_empty (!heap))
                then ( 
                    heap := (!heap).(0) <- el ;
                    downheap 0 heap
                );
                Some(v)

        
        let find (pred : value_type -> bool) (heap : t) : heap_index =
            Array.findi pred (!heap)

        let remove (index : heap_index) (heap : t) : unit = 
            let len = Array.length (!heap) in
            let el = (!heap).(len - 1) in
            heap := Array.remove (len - 1) 1 (!heap);
            if len - 1 != index 
            then (
                heap := (!heap).(index) <- el;
                upheap index heap;
                downheap index heap
            )

    end