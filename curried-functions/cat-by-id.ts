/*
 * This file is implemented follow concept in Maths4WP
 * Day 2: Currying (Slide 421-475)
 */

type Cat = string
type DbOptions = { where: { id: string } }

const myDB = {
    get(table: string, { where }: DbOptions): Cat {
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

const cat1 = catByID(getCatFromDB(myDB))('17')
const cat2 = catByID(getCatFromAPI(myAPI))('13')
// Of course, it (pointlessly) works with itself
const cat3 = catByID(catByID(getCatFromAPI(myAPI)))('19')
const cat4 = catByID(mockGetCat)('21')

console.log('From DB', cat1) // From DB cats-17
console.log('From API', cat2) // From API cats/13
console.log('From API', cat3) // From API cats/19
console.log('From Mock', cat4) // From Mock mockGetCat-21
