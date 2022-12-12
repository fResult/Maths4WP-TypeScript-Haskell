interface IUser {
    id: number;
    name: string;
    status: string;
}
const mappedUsers = new Map<string, Omit<IUser, "name">>([
    ["theo", { id: 1, status: "Online" }],
    ["maple", { id: 2, status: "Offline" }]
]) //?

mappedUsers.set("mark", { id: 100, status: 'AFK' })

const users: IUser[] = [...mappedUsers].map(([name, { id, status }]) => {
    return {
        id,
        name,
        status
    }
})

console.log(users)
// generate user ids from mappedUsers
const userIdsInChat = [1, 7, 17, 17]

function isUserOnline(userId: number) {
    return userIdsInChat.includes(userId)
}

function removeUserFromId(userId: number) {
    return userIdsInChat.filter(id => id !== userId)
}

const userIdsInChatSet = new Set(userIdsInChat) // Set { 1, 7, 17 }
