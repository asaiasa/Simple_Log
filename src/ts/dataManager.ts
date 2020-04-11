import E_STORE from 'electron-store';
import { MINUTES, NEXT_AGENDA } from './class';

export default class dataManager {

    private store: E_STORE = new E_STORE();
    private new_key: string = 'new';
    private agenda_key: string = 'agenda';
    private minutes_key: string = 'minutes';
    private search_conditions_key: string = 'search_conditions';

    constructor() {

    }

    /**
     * used when debug mode
     * all items are output to console log.
     */
    public console_all = (): void => {
        if (this.store.has(this.new_key)) {
            console.log('key: ' + this.new_key);
            console.log(this.get_New());
        }
        if (this.store.has(this.agenda_key)) {
            console.log('key: ' + this.agenda_key);
            console.log(this.get_Agenda());
        }
        if (this.store.has(this.minutes_key)) {
            console.log('key: ' + this.minutes_key);
            console.log(this.get_Minutes());
        }
    }

    /**
     * Used when debug mode
     * all items are deleted.
     */
    public del_all = (): void => { this.store.clear(); }

    /**
     * set new(=temporal) members data
     */
    public set_New = (minutes: MINUTES) => { this.store.set(this.new_key, JSON.stringify(minutes)); }

    /**
     * set new minutes data as typeof MINUTES
     */
    public get_New = (): MINUTES | null => {
        if (this.store.has(this.new_key)) {
            var temp: any = JSON.parse(this.store.get(this.new_key));
            var minutes: MINUTES;
            minutes = new MINUTES(temp['member'], temp['date'], temp['id']);
            return minutes;
        }
        else return null;
    }

    /**
     * delete new(=temporal) data
     */
    public del_New = (): void => {
        if (this.store.has(this.new_key)) this.store.delete(this.new_key);
    }

    /**
     * set agenda list
     */
    public set_Agenda = (list: NEXT_AGENDA[]): void => { this.store.set(this.agenda_key, JSON.stringify(list)); }

    /**
     * get agenda list as typeof NEXT_AGENDA
     */
    public get_Agenda = (): NEXT_AGENDA[] => {
        if (this.store.has(this.agenda_key)) {
            var temp: any[] = JSON.parse(this.store.get(this.agenda_key));
            var agenda: NEXT_AGENDA[] = [];
            temp.forEach(elm => agenda.push(new NEXT_AGENDA(elm['name'], elm['date'], elm['id'])));
            return agenda;
        }
        else return [];
    }

    /**
     * set minutes
     */
    public set_Minutes = (minutes: MINUTES[]): void => { this.store.set(this.minutes_key, JSON.stringify(minutes)); }

    /**
     * get minutes as typeof MINUTES
     */
    public get_Minutes = (): MINUTES[] => {
        if (this.store.has(this.minutes_key)) {
            var temp: any[] = JSON.parse(this.store.get(this.minutes_key));
            var minutes: MINUTES[] = [];
            temp.forEach(elm => minutes.push(new MINUTES(elm['member'], elm['date'], elm['id'], elm['agenda'], elm['content'], elm['conclusion'], elm['reason'], elm['memo'], elm['completed'])));
            return minutes;
        }
        else return [];
    }

    /**
     * return whether or not search key exsits
     */
    public chk_search = (): boolean => { return this.store.has(this.search_conditions_key); }

    /**
     * set searched conditions
     * text-condition(word1, word2, ...);start_date;end-date
     */
    public set_searched = (s: string): void => { this.store.set(this.search_conditions_key, JSON.stringify(s)); }

    /**
     * get searched conditions
     * return as string array
     */
    public get_searched = (): string[] => {
        if (this.store.has(this.search_conditions_key)) return (JSON.parse(this.store.get(this.search_conditions_key)) as string).split(';');
        else return [];
    }

    /**
     * delete search key
     */
    public del_search = (): void => { if (this.store.has(this.search_conditions_key)) this.store.delete(this.search_conditions_key); }

}
