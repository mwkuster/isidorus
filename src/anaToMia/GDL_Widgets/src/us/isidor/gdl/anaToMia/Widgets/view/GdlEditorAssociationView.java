package us.isidor.gdl.anaToMia.Widgets.view;

import java.util.ArrayList;

import com.google.gwt.core.client.JsArray;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;


public class GdlEditorAssociationView extends GdlAssociationView {
	protected ArrayList<Association> associationsToRepresent = null;
	protected boolean associationsToRepresentSet = false;
	

	public GdlEditorAssociationView(Topic tmRepresentative, Topic receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException {
		super(tmRepresentative, receivedData, gdlParent);
		
		// create an association item for each association that must be represetned
		// by this view
		for (int i = 0; i != this.getAssociationsToRepresent().size(); ++i){
			this.addToContainerPanel(new AssociationItem(this.tmRepresentative, this.getAssociationsToRepresent().get(i), this));
		}
	}

	
	// returns all associations of the received topic that have a
	// correct type determined by this association-view
	private ArrayList<Association> filterAssociationsByType() throws InvalidGdlSchemaException {
		ArrayList<Association> allAssociations = new ArrayList<Association>();
		if(this.receivedData != null)
			for(int i = 0; i != ((Topic)this.receivedData).getRolesPlayed().length(); ++i)
				allAssociations.add(((Topic)this.receivedData).getRolesPlayed().get(i).getParent());
		
		// collect only the associations that match the correct association type
		ArrayList<Association> filteredAssociationsByType = new ArrayList<Association>();
		for (Association assoc : allAssociations)
			if(assoc.getType().equals(this.getAssociationType())) filteredAssociationsByType.add(assoc);
		
		return filteredAssociationsByType;
	}
	
	
	// returns all associations contained in the ArrayList allAssociations
	// that have the only permitted role types
	private ArrayList<Association> filterAssociationsByRoleAndPlayerTypes(ArrayList<Association> allAssociations) throws InvalidGdlSchemaException {
		if(allAssociations == null) return new ArrayList<Association>();
		
		ArrayList<Association> filteredAssociationsByAssociationRoleConstraints = new ArrayList<Association>();
		for (Association assoc : allAssociations){
			JsArray<Role> roles = assoc.getRoles();
			int i = 0;
			for( ; i != roles.length(); ++i){
				Topic rt = roles.get(i).getType();
				Topic rp = roles.get(i).getPlayer();

				int j = 0;
				for( ; j != this.getRoleAndPlayerTypes().size(); ++j){
					Pair<Topic, Topic> pair = this.getRoleAndPlayerTypes().get(j);
					if(pair.getFirst().equals(rt) && TmHelper.isInstanceOf(rp, pair.getSecond())) break;
				}

				if(j == this.getRoleAndPlayerTypes().size()) break;
			}
			
			if(i == roles.length())filteredAssociationsByAssociationRoleConstraints.add(assoc);
		}
		
		return filteredAssociationsByAssociationRoleConstraints; 
	}
	

	// throws an InvalidGdlSchemaException if passed one association does not satisfy
	// any defined role-combination-constraint
	private void checkAssociationsByRoleCombinations(ArrayList<Association> allAssociations) throws InvalidGdlSchemaException {
		if(this.getRoleCombinationConstraints().size() == 0) return;
		
		for (Association assoc : allAssociations) {
			JsArray<Role> roles = assoc.getRoles();
			if(roles.length() != 2) throw new InvalidGdlSchemaException("the association " + assoc + " violates the defined role-combination-constrains");
			Role fstRole = roles.get(0);
			Role sndRole = roles.get(1);
			
			int i = 0;
			for ( ; i != this.getValidRoleCombinations().size(); ++i) {
				Pair<Topic, Topic> fstCombi = this.getValidRoleCombinations().get(i).getFirst();
				Pair<Topic, Topic> sndCombi = this.getValidRoleCombinations().get(i).getSecond();
				
				if(fstRole.getType().equals(fstCombi.getFirst()) && TmHelper.isInstanceOf(fstRole.getPlayer(), fstCombi.getSecond()) &&	sndRole.getType().equals(sndCombi.getFirst()) && TmHelper.isInstanceOf(sndRole.getPlayer(), sndCombi.getSecond())) break;
				if(sndRole.getType().equals(fstCombi.getFirst()) && TmHelper.isInstanceOf(sndRole.getPlayer(), fstCombi.getSecond()) &&	fstRole.getType().equals(sndCombi.getFirst()) && TmHelper.isInstanceOf(fstRole.getPlayer(), sndCombi.getSecond())) break;
			}
			
			if(i == this.getValidRoleCombinations().size()) throw new InvalidGdlSchemaException("the association " + assoc + " violates the defined role-combination-constrains");
		}
	}
	
	
	// returns an array with associations that
	// must be represented by this association view
	private ArrayList<Association> filterAssociations() throws InvalidGdlSchemaException {
		ArrayList<Association> filteredAssociations = this.filterAssociationsByType();
		filteredAssociations = this.filterAssociationsByRoleAndPlayerTypes(filteredAssociations);
		this.checkAssociationsByRoleCombinations(filteredAssociations);
		this.associationsToRepresent = filteredAssociations;
		this.associationsToRepresentSet = true;
		return this.associationsToRepresent;
	}
	
	
	// Returns the associations that are represented by this associaton-view
	public ArrayList<Association> getAssociationsToRepresent() throws InvalidGdlSchemaException {
	  	if(this.associationsToRepresentSet) return this.associationsToRepresent;
		else return this.filterAssociations();
	}
}
