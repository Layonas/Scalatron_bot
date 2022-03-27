package scalatron.botwar.botPlugin.Layonas
import java.util.Random
import scala.collection.immutable._
import scala.util.control.Breaks._
import scala.annotation.meta.param


object left{
    def apply() = {
        val left = -1
        left
    }
}

object right{
    def apply() = {
        val right = 1
        right
    }
}

object down{
    def apply() = {
        val down = 1
        down
    }
}

object up{
    def apply() = {
        val up = -1
        up
    }
}

object utils{
    def XY() = {
        val x = new Random().nextInt(3)-1
        val y = new Random().nextInt(3)-1
        val dir = x+":"+y
        (dir, x, y)
    }
    def Move(cords: XY) = {
        val dir = "Move(direction=" + cords.x + ":" + cords.y + ")"
        dir
    }
    def RandMove() = {
        val x = new Random().nextInt(3)-1
        val y = new Random().nextInt(3)-1
        val dir = "Move(direction=" + x + ":" + y + ")"
        dir
    }

    def heuristic(a: XY, b: XY) = {
        val dist = a.distanceTo(b)
        dist.toFloat
    }

    def contains(grid: Array[Array[XY]], element: XY) = {
        var flag = false
        breakable{
            for(x <- 0 until grid.length){
                for(y <- 0 until grid.length){
                    if(grid(x)(y).equals(element))
                    {
                        flag = true
                        break
                    }
                }
            }
        }
        flag
    }

    def containsArr(array: List[XY], element: XY) = {
        var flag = false
        breakable { 
            for(i <- 0 until array.length){
                if(array(i).equals(element)){
                    flag = true
                    break
                }

            }
        }
        flag
    }

    def A_star(start: XY, end: XY, grid: Array[Array[XY]], open: List[XY], closed: List[XY], dodge: List[Char]) : XY = {

        var count = 0
        var current: XY = null
        var openSet = open
        //var closedSet = Array.ofDim[XY](grid.length, grid.length)
        var closedSet = closed
        //println("Default array values are: " + closedSet(0)(0))
        openSet(0).g = 0
        openSet(0).f = openSet(0).g + heuristic(openSet(0), end)
        //println(openSet(0).f)
        //println("Open has neighbors: " + open(0).neighbors.length)
       // println("OpenSet has neighbors: " + openSet(0).neighbors.length)

        breakable { while(openSet.length > 0){

            //println("OpenSet is not empty")

            var best = 0
            for(i <- 0 until openSet.length){
                if(openSet(i).f < openSet(best).f){
                    best = i
                    //println("New best found: " + best)
                }
            }

            current = openSet(best)
            //println("Current node has neighbors: " + current.neighbors.length)

            //println("Checking whether: " + current + " equals " + end + "?" + current.equals(end))
            if(current.equals(end)){
                //println("closedSet contains: " + closedSet)
                "Status(text= Got food!)"
                break
            }

            openSet = openSet diff List(current)
            closedSet = closedSet :+ current
            // println("ClosedSet has values: " + closedSet)

            var neighbors = current.neighbors
            //println("Neighbor count: " + neighbors.length)

            for(i <- 0 until neighbors.length){
                //println("Current is: " + current)
                var neighbor = neighbors(i)
                //println(neighbor)
                //println(utils.containsArr(closedSet, neighbor))
                if(utils.containsArr(closedSet, neighbor) == false && !dodge.contains(neighbor.value)){

                    var tempG = current.g + heuristic(neighbor, current)
                    //println(neighbor + " is a valid position with g value: " + tempG + ", while neighbor g value is: " + neighbor.g)

                    var newPath = false
                    //println(utils.containsArr(openSet, neighbor) == true)
                    if(utils.containsArr(openSet, neighbor) == true){
                        //println("openSet contains neighbor: " + neighbor)
                        if(tempG < neighbor.g){
                            neighbor.g = tempG
                            newPath = true
                        }
                    } else{
                        neighbor.g = tempG
                        newPath = true
                        openSet = openSet :+ neighbor
                    }

                    if(newPath){
                        count += 1
                        neighbor.h = heuristic(neighbor, end)
                        neighbor.f = neighbor.g + neighbor.h
                        neighbor.prev = current
                    }
                }
            }
        } 
    }

        var path = List[XY]()
        var temp = current
        path = path.:+(temp)
        while(temp.prev != null){
            //println(temp.prev)
            path = path.:+(temp.prev)
            temp = temp.prev
        }

        // println(count)

        // println(path)

        return path(path.length-2)

        
    }

    def FoodRoutes(view: View, start: XY, grid: Array[Array[XY]], openSet: List[XY], closedSet: List[XY]): String = {
        val dodge_all = List('W', '?', 'm', 's', 'b', 'p')
        val missile_dodge = List('W', '?', 's', 'm', 'P', 'p', 'B')
        val no_moves = List('W', 'm', 's', 'b', 'p')

        view.CordsOfFood('P') match {
            case Some(offset) =>
                var next_move_location = utils.A_star(start, offset, grid, openSet, closedSet, dodge_all)
                var next_move = (next_move_location - start).signum
                //println("The food is at: " + offset + ", and we are moving: " + next_move + " towards: " + next_move_location)
                return "Status(text= NIAM NIAM) |" +    
                utils.Move(next_move)
            case None =>
                view.CordsOfFood('B') match {
                    case Some(offset) => 
                        var next_move_location = utils.A_star(start, offset, grid, openSet, closedSet, dodge_all)
                        var next_move = (next_move_location - start).signum
                        //println("The food is at: " + offset + ", and we are moving: " + next_move + " towards: " + next_move_location)
                        return "Status(text= Where the green stuff) |" +    
                        utils.Move(next_move)                                    
                    case None =>
                        view.CordsOfFood('?') match {
                            case Some(offset) =>
                                var next_move_location = utils.A_star(start, offset, grid, openSet, closedSet, no_moves)
                                var next_move = (next_move_location - start).signum
                                //println("The food is at: " + offset + ", and we are moving: " + next_move + " towards: " + next_move_location)
                                return "Status(text= Help Im lost) |" +    
                                utils.Move(next_move)
                            
                            case None =>
                                return utils.RandMove()
                        }
                }
        }
        return ""
    }

    def sum(cords: XY) = {
        cords.x.abs + cords.y.abs
    }

}

class ControlFunctionFactory {
    //
    def create = new Bot().respond _
}

class Bot {
    val dodge_all = List('W', '?', 'm', 's', 'b', 'p')
    val missile_dodge = List('W', '?', 's', 'b', 'P', 'p', 'B')
    var rocket_count = 0
    def respond(input: String) = {
        val (opcode, params) = CommandParser(input)
        val rand = new Random()
        var n = 0

        if(opcode == "React"){
            val unitMap = params("view")
            val generation = params("generation").toInt
            val energy = params("energy").toInt
            val slaves = params("slaves").toInt

            val view = View(unitMap)
            val grid = view.grid
            for(i <- 0 until grid.length){
                for(j <- 0 until grid.length){
                    grid(i)(j).addNeighbors(grid)
                }
            }

            val start = grid(view.center.x)(view.center.y)
            var openSet = List(start)
            var closedSet = List[XY]()

            var route = utils.FoodRoutes(view, start, grid, openSet, closedSet)
                
            if(generation > 0){

                val role = params("roles")

                if(role == "collector" && energy > 500){
                    val master = XY(params("master").split(":")(0).toInt,params("master").split(":")(1).toInt).signum
                    
                    view.CordsOfFood('M') match {
                        case Some(offset) =>
                            var next_move_location = utils.A_star(start, offset, grid, openSet, closedSet, dodge_all)
                            var next_move = (next_move_location - start).signum
                            println("Returning to master")
                            route = "Status(text= Master pls) |" +    
                            utils.Move(next_move)
                        case None =>
                            route = utils.Move(master)
                    }
                }
                if(role == "pop"){
                    println("popas")
                    view.CordsOfFood('m') match {
                    case Some(offset) =>
                        println("offsetas: " + offset)
                        println(start)
                        var next_move_location = utils.A_star(start, offset, grid, openSet, closedSet, missile_dodge)
                        println("location: " + next_move_location)
                        var next_move = (next_move_location - start).signum
                        println("move: " + next_move)
                        if(utils.sum(offset - start) <= 2 && utils.sum(offset - start) >= -2){
                            route = "Explode(size=2)"
                            rocket_count = 0
                        } else {
                            route = "Status(text=IM COMING FOR YOU)" + "|" + utils.Move(next_move)
                        }
                    case None =>
                        ""
                    }
                }
            }

            var slave_spawn = false
            var spawn = "Spawn()" 
            if((slaves == 0 && energy > 200 || (slaves != 0 && energy / slaves > 200)) && generation == 0){
                val (dir, x, y) = utils.XY
                spawn = "Spawn(direction=" + dir + 
                    ", name=killer" + slaves+1 + ",roles=collector)"
                slave_spawn = true
            }

            if(generation == 0 && energy > 200 && rocket_count == 0){
                view.CordsOfFood('m') match {
                    case Some(offset) =>
                        println("Found enemy")
                        var nml = utils.A_star(start, offset, grid, openSet, closedSet, missile_dodge)
                        var nm = (nml-start).signum
                        //spawn_rocket = "Spawn(direction="+nm+",roles=missile, name=asd"+rocket_count+",energy=100)"
                        spawn = "Spawn(direction=1:1,roles=pop,energy=200)"
                        slave_spawn = true
                        //rocket_count += 1 
                    case None =>
                        ""
                }
            }

            if(slave_spawn){
                println(spawn + " | " + route)
                spawn + "|" + route
            } else{
                route
            } 
        } else{
            println(opcode)
            ""
        }
    }
    
}


object CommandParser{
    def apply(command: String) = {
        def splitParam(param: String) = {
            val segments = param.split('=')
            if(segments.length != 2)
                throw new IllegalStateException("invalid key/value pair" + param)
            (segments(0), segments(1))
        }
        
        val segments = command.split('(')
        if(segments.length != 2)
            throw new IllegalStateException("invalid command: " + command)
            
        val params = segments(1).dropRight(1).split(',')
        val pairs = params.map(splitParam).toMap
        (segments(0), pairs)
    }
}

case class View(cells: String){

    val size = math.sqrt(cells.length).toInt
    val center = XY(size/2, size/2)
    val grid = Array.ofDim[XY](size, size)
    def w_map() = {
        for(i <- 0 until cells.length){
            val x = i % size
            val y = i / size
            var tile = XY(x, y)
            tile.value = cells.charAt(i)
            grid(x)(y) = tile
        }
        grid
    }
    
    def apply(relPos: XY) = cellAtRelPos(relPos); w_map()

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.apply(indexFromAbsPos(absPos))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells(indexFromRelPos(relPos))

    def CordsOfFood(c: Char) = {
        var nearest : Option[XY] = None
        var nearest_distance = Double.MaxValue
        for(i <- 0 until cells.length){
            if(c == cells(i)){
                val pos = absPosFromIndex(i)
                val dist = pos.distanceTo(center)
                if(dist < nearest_distance){
                    nearest_distance = dist
                    nearest = Some(pos)
                }
            }
        }
        nearest
    }

}

case class XY(x: Int, y: Int) {

    var f: Float = 0//Float.MaxValue
    var g: Float = 0//Float.MaxValue
    var h: Float = 0
    var value: Char = ' '

    var neighbors = List[XY]()

    var prev: XY = null

    def addNeighbors(grid: Array[Array[XY]]) = {
        if(x < grid.length - 1){
            neighbors = neighbors :+ grid(x+1)(y)
        }
        if(x > 0){
            neighbors = neighbors :+ grid(x-1)(y)
        }
        if(y < grid.length-1){
            neighbors = neighbors :+ grid(x)(y+1)
        }
        if(y > 0){
            neighbors = neighbors :+ grid(x)(y-1)
        }
        if(x > 0 && y > 0){
            neighbors = neighbors :+ grid(x-1)(y-1)
        }
        if(x < grid.length-1 && y > 0){
            neighbors = neighbors :+ grid(x+1)(y-1)
        }
        if(x > 0 && y < grid.length - 1){
            neighbors = neighbors :+ grid(x-1)(y+1)
        }
        if(x < grid.length - 1 && y < grid.length - 1){
            neighbors = neighbors :+ grid(x+1)(y+1)
        }
        //neighbors
        //println(neighbors.length)
    }

    def equals(other: XY) = x == other.x && y == other.y

    def isNonZero = x!=0 || y!=0
    def isZero = x==0 && y==0
    def isNonNegative = x>=0 && y>=0
    
    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x+dx, y)
    def addToY(dy: Int) = XY(x, y+dy)

    def +(pos: XY) = XY(x+pos.x, y+pos.y)
    def -(pos: XY) = XY(x-pos.x, y-pos.y)
    def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)

    def distanceTo(pos: XY) : Double = (this-pos).length
    def length : Double = math.sqrt(x*x + y*y)

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)
    override def toString = x + ":" + y
}


