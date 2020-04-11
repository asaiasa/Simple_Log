import { MINUTES, VIEW_DIALOG, SEARCH_DIALOG } from './class';
import { commonFunction as CF } from './common';
import { subcommon as SCF } from './subcommon';
import DM from './dataManager';
import { view_id } from './id';

const dm: DM = new DM();
var minutes: MINUTES[] = [];

window.onload = (): void => {
    CF.addEvent(view_id['search_btn'], search_minutes_condition);
    minutes = dm.get_Minutes(); // read minutes data
    listup_minutes(searched_check());   // search condition check and list up minutes
}

window.onunload = (): void => {
    var hidden_tag: HTMLElement | null = document.getElementById(view_id['hidden_id']);
    if (hidden_tag != null) {
        if (hidden_tag.innerText != '') {
            dm.set_searched(hidden_tag.innerText);
            hidden_tag.innerText = '';
        }
    }
}

/**
 * check list-up conditions
 */
const searched_check = (): number[] => {
    if (dm.chk_search()) {
        const cond: string[] = dm.get_searched(); // [0]=word1 word2 ...[1]start_date[2]end_date
        dm.del_search();
        return SCF.search_Minutes(minutes, cond);
    } else return SCF.arrayInit(minutes.length);
}

/**
 * list up minuts data
 * @param searched_condition - displayed minutes data index array
 */
const listup_minutes = (searched_condition: number[]): void => {
    const tbl: HTMLTableElement = document.getElementById(view_id['tbl_id']) as HTMLTableElement;
    var row: HTMLTableRowElement, cell: HTMLTableDataCellElement;
    var agenda: string[] = [];
    var disp_agenda: string = '';

    searched_condition.forEach(elm => {

        // make new row
        row = tbl.insertRow(-1);

        // date
        cell = row.insertCell(-1);
        cell.appendChild(document.createTextNode(minutes[elm].get_date_s('.')));

        // agenda list
        agenda = minutes[elm].get_agendas();
        disp_agenda = "<ol>";
        agenda.forEach(elm => {
            disp_agenda += '<li>' + elm + '</li>';
        })
        disp_agenda += "</ol>";
        cell = row.insertCell(-1);
        cell.innerHTML = disp_agenda;

        // View
        cell = row.insertCell(-1);
        cell.appendChild(add_view_btn(minutes[elm].get_id()))
        add_view_btn_event(minutes[elm].get_id(), elm);   // +1 is counted header row

    });

}

/**
 * create view button(icon)
 * @param id - given automatically
 */
const add_view_btn = (id: string): HTMLElement => {
    var icon: HTMLElement = document.createElement('a');
    icon.className = view_id['icon_class'];
    icon.innerHTML = view_id['icon_html'];
    icon.id = id;
    return icon;
}

/**
 * add click event tot view button(icon)
 * @param id - given automatically
 * @param row_index - given automatically
 */
const add_view_btn_event = (id: string, row_index: number): void => {
    let view_btn: HTMLElement | null = document.getElementById(id);
    if (view_btn != null) view_btn.addEventListener('click', (): void => { view_Minutes(row_index); });
}

/**
 * show modal window
 * @param row_index - given automatically
 */
const view_Minutes = (row_index: number): void => {
    const vd: VIEW_DIALOG = new VIEW_DIALOG();
    vd.rendererView(minutes[row_index]);
}

/**
 * show modal winwow
 */
const search_minutes_condition = (): void => {
    const sd: SEARCH_DIALOG = new SEARCH_DIALOG(view_id['hidden_id']);
    sd.dialogOpen();
}
