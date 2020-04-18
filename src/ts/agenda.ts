import { NEXT_AGENDA } from './class';
import { commonFunction as CF } from './common';
import DM from './dataManager';
import { agenda_id } from './id';

const dm: DM = new DM();
var Agenda_List: NEXT_AGENDA[] = [];

/**
 * Initilize
 */
window.onload = (): void => {
    CF.addEvent(agenda_id['add_btn'], add_table);
    display_change();
    CF.addEvent(agenda_id['checkbox'], display_change, 'change');   //call after display_change()
    listup_agenda() // list up agenda
}

/**
 * Store data
 */
window.onunload = (): void => {
    if (Agenda_List.length != 0)
        dm.set_Agenda(Agenda_List);
}

/**
 * list up agenda
 */
const listup_agenda = (): void => {
    Agenda_List = dm.get_Agenda();
    Agenda_List.forEach(elm => add_table(elm.get_name(), elm.get_date(), elm.get_id()));
}

/**
 * change display of calendar depending on chekcbox status
 */
const display_change = (): void => {
    ((document.getElementsByClassName('input-field col s6'))[0] as HTMLElement).style.display = CF.getCheckboxStatus(agenda_id['checkbox']) ? 'none' : 'block';
}

/**
 * add agenda to list and reflect to table
 */
function add_table(): void;
function add_table(name: string, schedule: string, id: string): void;
function add_table(name?: string, schedule?: string, id?: string): void {

    var agenda_name: string = "";
    var mtg_scheduled_date: string = "";
    var btn_id: string = "";

    if (name === undefined || schedule === undefined || id === undefined) {
        // get and reset objects
        agenda_name = CF.getText('title');

        if (CF.getCheckboxStatus(agenda_id['checkbox'])) mtg_scheduled_date = 'undecided';
        else {
            mtg_scheduled_date = CF.getCalendarDate('.', 'en');
            if (mtg_scheduled_date == "..") mtg_scheduled_date = 'undecided';
            CF.setCheckboxStatus(agenda_id['checkbox'], true);
            display_change();
        }
        CF.setText('title', '');

        // empty title is error
        if (agenda_name == "") return;

        // add to lists only when add agenda
        Agenda_List.push(new NEXT_AGENDA(agenda_name, mtg_scheduled_date));

        btn_id = Agenda_List[Agenda_List.length - 1].get_id();

    } else {
        agenda_name = name;
        mtg_scheduled_date = schedule;
        btn_id = id;
    }

    // get table
    const tbl: HTMLTableElement = document.getElementById('agenda_table') as HTMLTableElement;

    // add to table
    var row: HTMLTableRowElement = tbl.insertRow(-1);
    var cell: HTMLTableDataCellElement = row.insertCell(-1);
    cell.appendChild(document.createTextNode(agenda_name));
    cell = row.insertCell(-1);
    cell.appendChild(document.createTextNode(mtg_scheduled_date));
    cell = row.insertCell(-1);
    cell.appendChild(add_del_btn(btn_id));
    add_del_btn_event(btn_id);    // call after appendChild
}

/**
 * display delete button in table
 */
const add_del_btn = (id: string): HTMLElement => {
    var icon: HTMLElement = document.createElement('a');
    icon.className = agenda_id['del_btn_class'];
    icon.innerHTML = agenda_id['del_btn_html'];
    icon.id = id;
    return icon;
}

/**
 * add click event to delete button
 */
const add_del_btn_event = (id: string): void => {
    document.getElementById(id)?.addEventListener('click', (e: Event): void => { del_agenda(e); });
}

/**
 * function of clicking delete button.
 * delete the row
 * @param e - event variable, given automatically.
 */
const del_agenda = (e: Event): void => {
    // delete button
    var a_tag = (e.target as HTMLElement).parentElement;
    if (a_tag != null) {
        const tbl: HTMLTableElement = document.getElementById('agenda_table') as HTMLTableElement;
        const rows: HTMLCollectionOf<HTMLTableRowElement> = tbl.getElementsByTagName('tr');
        const row_length: number = rows.length;
        var delete_row_index: number = -1;
        var column: HTMLCollectionOf<HTMLTableDataCellElement>;
        for (let i = 1; i < row_length; ++i) {  // exclude header
            column = rows[i].getElementsByTagName('td');
            if (0 < column[2].innerHTML.indexOf(a_tag.id)) {
                delete_row_index = i;
                break;
            }
        }
        if (delete_row_index != -1) {
            tbl.deleteRow(delete_row_index);    // delete from table
            Agenda_List.splice(delete_row_index - 1, 1);
        }
    }
}
