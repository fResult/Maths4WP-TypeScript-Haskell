type Cat = string;
type DbOptions = { where: { id: string } };

const myDB = {
  get: (table: string, { where }: DbOptions) => `${table}-${where.id}`,
};
const myAPI = {
  get: (id: string) => `cats/${id}`,
};

function getCatFromDB(db: typeof myDB) {
  return function getCatByID(catID: string) {
    return db.get("cats", {
      where: {
        id: catID,
      },
    });
  };
}
function getCatFromAPI(api: typeof myAPI) {
  return function getCatByID(catID: string): Cat {
    return api.get(catID);
  };
}

function catByID(getCatByID: (catID: string) => Cat) {
  return function byID(id: string): Cat {
    return getCatByID(id);
  };
}

const cat1 = catByID(getCatFromDB(myDB))("17");
const cat2 = catByID(getCatFromAPI(myAPI))("13");
console.log("From DB", cat1); // From DB cats-17
console.log("From API", cat2); // From API cats/13
