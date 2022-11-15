/*
 * This file is implemented follow concept in Maths4WP
 * Day 2: Currying (Slide 421-475)
 */

type Cat = string
type QueryOptions = { where: { id: string } }

const myDB = {
    get(table: string, { where }: QueryOptions): Cat {
        return `${table}-${where.id}`
    }
}
const myAPI = {
    get(id: string): Cat {
        return `cats/${id}`
    }
}
function mockGetCat(id: string): Cat {
    return `mockGetCat-${id}`
}

function getCatFromDB(db: typeof myDB): (id: string) => Cat {
    return function getCatByID(catID: string): Cat {
        return db.get('cats', {
            where: {
                id: catID
            }
        })
    }
}
function getCatFromAPI(api: typeof myAPI): (id: string) => Cat {
    return function getCatByID(catID: string) {
        return api.get(catID)
    }
}

/*
 *      Domain    ->    Co-domain
 * (CatID -> Cat) -> (CatID -> Cat)
 */
function catByID(getCatByID: (catID: string) => Cat) {
    return function byID(id: string): Cat {
        return getCatByID(id)
    }
}

const cat13 = catByID(getCatFromAPI(myAPI))('13')
const cat17 = catByID(getCatFromDB(myDB))('17')

// Of course, it (pointlessly) works with itself
const cat19 = catByID(catByID(getCatFromAPI(myAPI)))('19')
const cat21 = catByID(mockGetCat)('21')

console.log('From API', cat13) // From API cats/13
console.log('From DB', cat17) // From DB cats-17
console.log('From API', cat19) // From API cats/19
console.log('From Mock', cat21) // From Mock mockGetCat-21
