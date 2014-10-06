#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Lieu : UVSQ
// Cadre : M1 Informatique -- UE Simulation


// 1. MACROS et CONSTANTES

// Macro qui renvoie une valeur aleatoire
// qui suit une loi uniforme sur [0,1[
#define uniforme ( (double)random()/(double)RAND_MAX )


// Macro qui renvoie une valeur aleatoire
// qui suit une loi exponentielle de parametre x
#define expo(x)(-log(uniforme) / (x) )


//constantes a fixer soi-meme
#define NMAX    30000
#define MU      8
#define NORDI   10
#define epsilon 0.05  //pour le test de convergence
#define K       1000   // pour le test de convergence




// 2. GESTION DE L'ECHEANCIER
//ACM arrivé du client sur la Machine
//DSM debut de service  du client sur la Machine
//FSM Fin de service  du client sur la Machine


typedef enum {ACM,DSM,FSM} type_evenement;  //remplir la liste d'événements

typedef struct evenement evenement;
struct evenement
{
  type_evenement type;
  int numero_file;
  double date;
  int numeroc;//numero client
  int numero_du_poste;
  struct evenement *nxt;
};



//Un type Echeancier est une liste d'évènements
typedef evenement* Echeancier;

//Ajoute un evenement trié par date dans l'échéancier
void ajouter(Echeancier *echeancier, double date, type_evenement type, int numero_file,int numero_poste,int numc){
  evenement* nouvelEvenement = malloc(sizeof(evenement));
  Echeancier listeAvant = NULL;
  Echeancier listeApres = *echeancier;

  nouvelEvenement->date = date;
  nouvelEvenement->numeroc=numc;
  nouvelEvenement->type = type;
  nouvelEvenement->numero_du_poste=numero_poste;
  nouvelEvenement->numero_file = numero_file;
  
   

  while((listeApres != NULL) && (listeApres->date < date)){
    listeAvant = listeApres;
    listeApres = listeApres->nxt;
     
  }
  nouvelEvenement->nxt = listeApres;
  if(listeAvant != NULL) listeAvant->nxt = nouvelEvenement;
  else *echeancier = nouvelEvenement;


}
// return une valeur reelle entre l'intervale a et b
 double uniforme_ab(double a, double b){
  double c=rand()/(double)RAND_MAX;
  
 
  return (b-a)*c+a;
}
//retourne l'entier le plus proche du reel
long arrondir(double a)
{
    long c=(long)a;
    if(c+0.5<=a)
        return c+1;
    else
        return c;
}
//return un entier compris entre l'interval a et b
long randomInteger(long a,long b)
{
    double x=uniforme_ab(a*1.0,b*1.0);
    return arrondir(x);
}
//Extrait l'événement en tete de l'échéancier
evenement* extraire(Echeancier *echeancier){
  evenement* nouvelEvenement = *echeancier;

  nouvelEvenement = *echeancier;
  *echeancier = (*echeancier)->nxt;
  return nouvelEvenement;
}


// histo1 permet de calculer la distribution  de probabilité du mode 1

void histo1(FILE* F, double* valeurs,double lambda,  int nb_classes,int Nmax){

	int Max=-1;
	double moyenne=0;
	
	unsigned int i;
	int j=0;

 
      //On compte le nombre  d'éléments "a" dans valeurs[i] telle que i(minute)<a<=i+1(mimunte)
	for(i=0;i<Nmax;i++)
	{
		  if(Max<=valeurs[i])
		  Max=valeurs[i];
	}
	     
  fprintf(F,"f=%d   \n",Max);
int liste[Max+1];

for(i=0;i<Max+1;i++)
liste[i]=0;
  // //On compte le nombre  d'éléments "a" dans valeurs[i] telle que i(minute)<a<=i+1(mimunte)
	for(i=0;i<Nmax;i++)
	{
		
		
				j=valeurs[i];
				
		       liste[j]=liste[j]+1;
		    
	    
	}
	double s =0.0;
	double l=0.0;
	

	       for(i=0;i<Max+1;i++)
	        {
		
	    fprintf(F,"%d   %f\n",i,(double)liste[i]/Nmax);
	    
		
		s=s+(double)liste[i]/Nmax;
		
		moyenne=moyenne+(double)i*liste[i]/Nmax;
		
		  if(i<40)
		  l=l+(double)liste[i]/Nmax;
		  
		  
		
		
		
		
		
		
	}
	printf("s=%f\n",s);
	fprintf(F,"moyenne=%f,\n",moyenne);
	fprintf(F,"repart=%f,\n",l);
	
}



void freeEcheancier(Echeancier echeancier){
  Echeancier temp;
  while(echeancier){
    temp = echeancier;
    echeancier = echeancier->nxt;
    free(temp);
  }
}

// 3. SIMULATION

//fonction valeur absolue
double abso(double x){
  if(x>0) return x;
  else return -x;
}




//renvoie le premier  numéro du poste non occupé selon l'ordre croissant des numéros de poste allant de 0 à 9
int poste_libre(int *poste)
{
	int i;
	int numposte=-1;
	int d=1;//booleen
	
	
	for(i=0;i<NORDI;i++)
	     {
			   if(poste[i]!=-1 && d==1)
			       { 
					  
					   numposte=i;
					   d=0;
				   }
	          
	          
		      
		  }
		  
		  return numposte;
}


// Fonction qui lance une simulation

void simul(FILE *F,double lambda,int mode,int distrib)  //mode 1, 2, ou 3 pour les 3 situations distrib à 0 pour calculer le temps moyen d'attente et à 1 pour calculer la distribution du temps d'attente
{

  //Déclaration des variables

    
    double nombre_cumule_clients_total ;//temps total cumulé des client 
    
    int poste_occupe[NORDI];//dit si le poste i(en indice) est occupé ou libre:valeur possible:1,0
    
    long  Nb_clients_total;//nombre de client total 
    
    long  nb_clients_dans_la_file;//mode 1--nombre de client dans la file d'attente
    
    long nombre_maximal_file_wait;//nombre de file d'attente
    
    long *Nbr_client_file;//pointe sur le nombre de client en attente dans la la file numéro j ,avec j l'indice du tableau allant de 1 à N(mode 2 et 3)
 
    int convergence;//critère d'arret
    
    int memo,min;//mode 3
    
    int j;
    
    long munero_client;//numero du client-mode 1,2,3
    
    long poste_alea;  // variable temporaire qui stock le numéro du poste affecté a un client-mode2
    
    int i;
    double DateA[NORDI][NMAX];//mode 2 et 3--Stock la date d'attente du client i(0<=i<NMAX) dans la file j(0<=j<NORDI)
    
    double TPA[NORDI][NMAX];//mode 2 et 3--stock la date d'arrivé du client i(0<=i<NMAX) dans la file j(0<=j<NORDI) et prend  la valeur -1 à chaque debut de service du client
 
    double numero_c_a[NMAX];//mode 1--stock la date d'arrivé du client i(0<=i<NMAX) dans la file et prend  la valeur -1 à chaque debut de service du client
    
    double Temps_AC[NMAX];//mode 1--Stock la date d'attente du client i(0<=i<NMAX) dans la file
   
    Echeancier echeancier=NULL;
   
    evenement *event;

  //Initialisation  de l'echeancier et des variables utiles


    ajouter(&echeancier,0.0,ACM,-1,1,-1); //date d'arrivée du prochain client.
    
    nombre_cumule_clients_total = 0;

    nb_clients_dans_la_file=0;
    
    munero_client=0;
    
    Nb_clients_total=0;
    
    convergence=0;
    
    poste_alea=0;
    
    
 
    
   //Initialisation  du tableau poste_occupe qui prend deux valeur:0(si le poste n'est pas occupé),1(si le poste l'est) 
   
   
    for(j=0;j<NORDI;j++) poste_occupe[j]=0;
    
		
    //Initialisation des variable neccessaire à la simulation selon les modes{1,2,3}     

    if(mode==1)
    {
        nombre_maximal_file_wait=-1;
        
        
        Nbr_client_file=(long*)malloc(2*sizeof(long));
        
    //Initialisation respective du  tableau "date d'attente du client" et "date d'arrivé
         
    for(j=0;j<NMAX;j++) numero_c_a[j]=-1.0;
    
    
    //Initialisation   du temps d'attente de chaque client..
  
   
    for(j=0;j<NMAX-1;j++) Temps_AC[j]=0;
    
    
    }
    
    
    if(mode==2 || mode==3 )
    {


        nombre_maximal_file_wait=10;
        
        Nbr_client_file=(long*)malloc(NORDI*sizeof(long));
        
        for(j=0;j<NORDI;j++) Nbr_client_file[j]=0;
        
      //Initialisation respective du  tableau "date d'attente du client" et "date d'arrivé du client"
 
    for(j=0;j<NORDI;j++)
	{
		 for(i=0;i<NMAX;i++)
		 {
            DateA[j][i]=-1.0;
    
            TPA[j][i]=-1.0;
    
         }
    }
    }


  //Boucle principale
  
  while (Nb_clients_total<30000) {  //critère d'arrêt





    
 
    event=extraire(&echeancier);   // on extrait le premier événement de l'échéancier pour le traiter
        if(event->type==ACM)
        {


         
         
         
          printf("********************************debut*************\n");
       // fprintf(F,"Arrive du client %ld  a la date %f \n", munero_client+1,event->date);
          printf("Arrive du client %ld  a la date %f \n", munero_client+1,event->date);
          printf("********************************fin*************\n");
         
            
             

         if(mode==1)
        {
				munero_client= munero_client+1;
			
				
          
          if(munero_client==30000)
          {
			  
			   
			//calcule le temps d'attente de chaque client restant dans la file lors de l'arrivé du dernier client		
					
			for(j=0;j<NMAX-1;j++)
			{
			if(numero_c_a[j]!=-1) Temps_AC[j]=event->date-numero_c_a[j];
		    }
						  
						  
					   				
		   }

               
      if(poste_libre(poste_occupe)!=-1)	//si il y'a un poste de libre affecte le poste au client qui vient d'arrivé
      {
		  
		      
		  ajouter(&echeancier, event->date,DSM, nombre_maximal_file_wait,poste_libre(poste_occupe)+1, munero_client);
	
	  }
		 
		 else
		 {
	
		     
		    nombre_maximal_file_wait=1;
		    
		       printf("********************************debut*************\n");
		       //fprintf(F,"Le client %ld  est en attente de poste libre depuis la date %f \n", munero_client,event->date);
		       //fprintf(F,"Le client %ld  est en attente de poste libre depuis la date %f \n", munero_client,event->date);
		       printf("********************************fin************\n");
		       printf("Le client %ld  est en attente de poste libre depuis la date %f \n", munero_client,event->date);
		  
		       printf("\n");
		 
               nb_clients_dans_la_file=nb_clients_dans_la_file+1;
            
           
               numero_c_a[munero_client-1]=event->date;  
           
            

		} 
	
            

           
            Nb_clients_total=Nb_clients_total+1;
             
            
           
            ajouter(&echeancier,event->date + expo(lambda),ACM,nombre_maximal_file_wait,munero_client+1,-1);
            
        }
        
        
        
        if(mode==2)
        {
			
			// choisis aléatoirement le numero d'un poste 
			
			 poste_alea=randomInteger(1,10);
			 
			 munero_client= munero_client+1;
			 
			 
			 if(munero_client==30000)
          {
			  
			   
				//calcule le temps d'attente de chaque client restant dans les files lors de l'arrivé du dernier client		
				
				  for(i=0;i<NORDI;i++)
				  {
					    for(j=0;j<NMAX;j++)
					    {
						
						
						   if(TPA[i][j]!=-1) DateA[i][j]=event->date-TPA[i][j];
						     
						    
					     }
				  }
				
		  }
		  
		  
		  
		  
		 if(munero_client!=30000)
		  {
			   if(poste_occupe[poste_alea-1]==0)
			      {	
					  
			     ajouter(&echeancier, event->date,DSM,-1,poste_alea-1, munero_client-1);
			     
			     
			      }
			        else
			        {
						
					  //fprintf(F,"Le client %ld  est en attente de poste libre depuis la date %f  dans la file %ld \n", munero_client,event->date,poste_alea);
					  
					  
					   Nbr_client_file[poste_alea-1]=Nbr_client_file[poste_alea-1]+1;
					   
					   
					   TPA[poste_alea-1][munero_client-1]=event->date;
					   
				     }
				 ajouter(&echeancier,event->date + expo(lambda),ACM,-1,-1,munero_client+1);
				 
				 
			 }
				
				 Nb_clients_total=Nb_clients_total+1;
			 
		
	}
	
	
	 if(mode==3)
        {
			          
			  munero_client= munero_client+1;
			  
			  
			   if(munero_client==30000)
          {
			  
			//calcule le temps d'attente de chaque client restant dans la file lors de l'arrivé du dernier client		
			   
					
				  for(i=0;i<NORDI;i++)
				  {
					  
					  for(j=0;j<NMAX;j++)
					  {
						
						
					  if(TPA[i][j]!=-1) DateA[i][j]=event->date-TPA[i][j];
						     
						  
					  }
				  }
				
		  }
			   
			   
			   
			   
			   if(munero_client!=30000)
		  {
			  
			  
      if(poste_libre(poste_occupe)!=-1)	
      {	
		  ajouter(&echeancier, event->date,DSM, nombre_maximal_file_wait,poste_libre(poste_occupe), munero_client-1);            
		
	  }
		 
		 else
		 {
			 
			 min=Nbr_client_file[0];
			 
			 
			
			        for(i=0;i<NORDI;i++)
			        {
						
			             if(min>=Nbr_client_file[i])
			             {
							 
			               min=Nbr_client_file[i];
			               
			               memo=i;
			              }
			             
				     }
			        
			 //fprintf(F,"Le client %ld  est en attente de poste libre depuis la date %f  dans la file %d \n", munero_client,event->date,memo+1); 
		     
		     Nbr_client_file[memo]=Nbr_client_file[memo]+1;
		     
		     TPA[memo][munero_client-1]=event->date;
		     
		      
	}
	
	
	 ajouter(&echeancier,event->date + expo(lambda),ACM,-1,-1,munero_client+1);
	 
	 
	 }
	 
	 
	  Nb_clients_total=Nb_clients_total+1;
	  
	  
	 }
	 
}

     
  if(event->type==DSM)
   {
							    
						 	
		
						
		 printf("********************************debut*************\n");
		  printf("Debut de service  du client %d  sur le poste %d a la date %f \n", event->numeroc+1,event->numero_du_poste,event->date);
		 //fprintf(F,"Debut de service  du client %d  sur le poste %d a la date %f \n", event->numeroc+1,event->numero_du_poste+1,event->date);
		  printf("********************************fin************\n");
		   printf("\n");
		   
      if(mode==1)
       {
			 
          ajouter(&echeancier, event->date + expo(MU),FSM,nombre_maximal_file_wait,event->numero_du_poste, event->numeroc);
          
	
	       if(numero_c_a[event->numeroc]!=-1.0)
	        {
	
	         //fprintf(F,"Le Temps d'attente du client %d  est  la date %f \n",  event->numeroc,event->date-numero_c_a[event->numeroc]);//date_client(echancier_date, event->numeroc));
	          Temps_AC[event->numeroc]=event->date-numero_c_a[event->numeroc];
	  
	            numero_c_a[event->numeroc-1]=-1.0;
            }

            else
               {
		            numero_c_a[event->numeroc-1]=-1.0;
		            
		            Temps_AC[event->numeroc-1]=0.0;
		            
	                printf("Le Temps d'attente du client %d  est  la date %f \n",  event->numeroc,0.00);
	            }
  
	   
	
	
                  poste_occupe[event->numero_du_poste-1]=-1;
 
      }
      
      
      
  
  if(mode==2 || mode==3)
  {
	   ajouter(&echeancier, event->date + expo(MU),FSM,-1,event->numero_du_poste, event->numeroc);
	   
	   
	   poste_occupe[event->numero_du_poste]=-1;
	   
	   
	   
	   if(TPA[event->numero_du_poste][event->numeroc]!=-1.0)
	   {
		   
		   DateA[event->numero_du_poste][event->numeroc]=event->date-TPA[event->numero_du_poste][event->numeroc];
	   
	        //fprintf(F,"Le Temps d'attente du client %d est %f \n",  (event->numeroc)+1,DateA[event->numero_du_poste][event->numeroc]);
	        
	       TPA[event->numero_du_poste][event->numeroc]=-1.0;
       }
         else
        {
              //fprintf(F,"Le Temps d'attente du client %d  est  la date %f \n",  (event->numeroc)+1,0.00);
        
               DateA[event->numero_du_poste][event->numeroc]=0;
               
               TPA[event->numero_du_poste][event->numeroc]=-1.0;
        }
	    
	           TPA[event->numero_du_poste][event->numeroc]=-1.0;
  }

	  

  }
  
  //Fn de service 

        if(event->type==FSM)
   {
	  
	    printf("********************************debut*************\n");
	  
	    //fprintf(F,"Le  client %d  libere le poste %d a la date %f \n", event->numeroc+1,event->numero_du_poste+1,event->date);
	    printf("Le  client %d  libere le poste %d  de la file a la date %f \n", event->numeroc+1,event->numero_du_poste+1,event->date);
	    printf("********************************fin*************\n");
	   
	   
	  // fin de service  pour le mode 1
            if(mode==1)
            {

                int t=1;
                i=1; 
                
                
                if(nb_clients_dans_la_file>0)
              {
                
                
                long k=0;
                
           // cette boucle permet de memoriser via l'entier k  le numero du  premier  client  en attente sur le poste qui s'est libéré   
                 while(t && i<=NMAX)
                 {
					  if(numero_c_a[i]!=-1.0)
					       {
							   k=i+1;
							   
							   t=0;
							
						   }
						   
					       else
					       
					       i++;   
				 }
                 
                  
                  
			      
       ajouter(&echeancier, event->date,DSM,nombre_maximal_file_wait,event->numero_du_poste,k);
	
                nb_clients_dans_la_file=nb_clients_dans_la_file-1;
                
			
              
               

			}
			else
			{
					
					
			poste_occupe[event->numero_du_poste-1]=0;
			 
		    }
			
			

               
               
           
  }
  
  // fin de service  pour le mode 2 ou 3
  
  if(mode==2 || mode==3)
  {
	  
	       
	   if(Nbr_client_file[event->numero_du_poste]>0)
	   {
		  int t=1,i=0,k=0;
		  
		  // cette boucle permet de memoriser via l'entier k  le numero du  premier  client  en attente sur le poste qui s'est libéré    
		  
		  
		    while(t && i<NMAX)
                 {
					 
					  if(TPA[event->numero_du_poste][i]!=-1.0 && t)
					       {
							   k=i;
							   t=0;
							  
						   }
					       else
					       i++;   
				 }
				 
				 
		   ajouter(&echeancier, event->date,DSM,-1,event->numero_du_poste,k);
		   
	      Nbr_client_file[event->numero_du_poste]=Nbr_client_file[event->numero_du_poste]-1;
				
	   }
	   else
			{
					
					
			poste_occupe[event->numero_du_poste]=0;
			 
		 }
  }
}
  
   
    
   
   
     free(event);
}


     
      Nb_clients_total=0;
      
      nombre_cumule_clients_total=0;
      
	  double tmp=0.0;
	  
	  
	  //Determination de la convergence du système selon  les modes spécifiques
	  
	  
	  if(mode==1)
	  {
		  
		  
		  // determination de la convergennce du systeme via la variable "convergence"
		  
		  
        while(convergence<10 && Nb_clients_total<NMAX)
        {
			
			
			//calculé le temps d'attente cummulé dans la file
			
			nombre_cumule_clients_total+=Temps_AC[Nb_clients_total];
			
		//calcule de la convergence à chaque  K client arrivé dans le systeme	
			
    if(Nb_clients_total % K == 0 && Nb_clients_total > 0)
    {
		
       if(abso((nombre_cumule_clients_total*1.0/ Nb_clients_total- tmp))  < epsilon)
      
	      convergence=convergence+1;
       else
       
	   convergence=0;
	
      tmp=nombre_cumule_clients_total*1.0/ Nb_clients_total;
    }
    
     Nb_clients_total=Nb_clients_total+1;
   
       }
}

if(mode==2 ||  mode==3)
{
	

   int l=0;
   
         while(Nb_clients_total<NMAX && convergence<10)

      {

      

      
        double k=-1.0;
       
       
       
     
                //détermine et stock dans k la date  d'attente du client l+1
 
               
                for(i=0;i<NORDI;i++)
                   {
					   
					   if(k<=DateA[i][l])
					   {
					      k=DateA[i][l];
					     
					     
					  }
					  
					      
				   }
				   
				   //calcule du temps d'attente cumule des clients
				   
				   
               nombre_cumule_clients_total=nombre_cumule_clients_total+k;
               
				   

             if(Nb_clients_total% K == 0 && Nb_clients_total>0)

       { 
	
               
	
	

                  if(abso((nombre_cumule_clients_total/Nb_clients_total)- tmp)< epsilon) 

                  convergence=convergence+1; 

                  else

                  convergence=0; 

                  tmp =nombre_cumule_clients_total*1.0/Nb_clients_total;


            }
            
            
       Nb_clients_total=Nb_clients_total+1;
       l++;
 
      }


  }
  

//DISTRIBUTION


      if(distrib){


// histo1 permet de calculer la distribution  de probabilité du mode 1


         if(mode==1)  histo1(F,Temps_AC,lambda,361,Nb_clients_total-1);
         
         
      //Calcule de la proabilité qu'un client attende entre i et i+1 mn   
      if(mode==2 || mode==3)
      {
		  
		  
	      int Max=-1;
	      int k=0;
	      double moyenne=0;
	
	
	

 
     // détermine le temps d'attente le plus élévé dans toutes les files d'attente confondu 
     
     
      for(j=0;j<NORDI;j++)
        {
	      for(i=0;i<Nb_clients_total-1;i++)
	         {
		        if(Max<=DateA[j][i]) Max=DateA[j][i];
		    
	         }
	    }
	    
	    
	    
	    
	    
	  int liste[Max+1],l;
	  
	  
   //initialisation du tableau liste qui stockera le nombre de client qui attente ente i et i+1 mn
        for(i=0;i<Max+1;i++)liste[i]=0;
        
        
         double m;
  
	
    
	     //On compte le nombre  de client j qui attendent entre i et i+1  avec 0<=i<Max telle que i(minute)<a<=i+1(mimunte)
	       for(i=0;i<Nb_clients_total-1;i++)
	       {
			   m=-1;
	        for(k=0;k<10;k++)
	        {
		
		
		
			if(m<=DateA[k][i]) m=DateA[k][i];
		
		
		
	}
	 l=m;  
		   
		   liste[l]=liste[l]+1;  
	
	    
}
double l1=0.0;
     // Ecrit  dans le fichier F la valeur de la probailité d'attente entre l'intervale i et i+1mn 
       for(i=0;i<Max+1;i++)
       {
		   
fprintf(F,"%d   %f\n",i,(double)liste[i]/(Nb_clients_total-1));

		moyenne=moyenne+(double) i*liste[i]/(Nb_clients_total-1);
		if(i<40)
		  l1=l1+(double)liste[i]/(Nb_clients_total-1);


       }
fprintf(F,"moyenne=%f,\n",moyenne);
fprintf(F,"repart=%f,\n",l1);
  
}

       
  }
  
  
  else
  
   {

  if(convergence==10)
    
   fprintf(F,"%f   %f   \n",lambda ,nombre_cumule_clients_total*1.0/Nb_clients_total);
   else
    fprintf(F,"%f   %f  \n",lambda ,-1.0);
    
    
    }


}





  // 4. r
  /*La fonction prend en argument un nombre positif a qui signifie:
    a = 0 : trace les temps moyens pour chaque mode quand lambda varie
    a > 0 : trace l'histogramme pour chaque mode où a est la valeur de lambda
   */
int main (int argc, char* argv[]){
  char *nom_fic_res1 = "resultats_simul1.data";
  char *nom_fic_res2 = "resultats_simul2.data";
  char *nom_fic_res3 = "resultats_simul3.data";
  FILE *F1,*F2, *F3,*F4;
 double lambda=0;

  if(argc !=2)
  {
    printf("Nombre d'arguments incorrect\n");
    exit(0);
  }


 lambda = atoi(argv[1]);

  F1 = fopen(nom_fic_res1,"w");
  if (F1==NULL){
    fprintf(stderr,"Pb ouverture : %s\n",nom_fic_res1);
    exit(1);
  }
  F2 = fopen(nom_fic_res2,"w");
  if (F2==NULL){
    fprintf(stderr,"Pb ouverture : %s\n",nom_fic_res2);
    exit(1);
  }
  F3 = fopen(nom_fic_res3,"w");
  if (F3==NULL){
    fprintf(stderr,"Pb ouverture : %s\n",nom_fic_res3);
    exit(1);
  }

 if(lambda==0)
  {
  for(lambda = 1; lambda <12; lambda +=0.2)
   {
      //simul(F1,lambda,1,0);
      /*simul(F2,lambda,2,0);*/
      simul(F3,lambda,3,0);
    }
  }
  
  else
  {
    //simul(F1,361,1,1);
   //simul(F2,361,2,1);
    //simul(F3,361,3,1);
  } 
  fclose(F1);
  fclose(F2);
  fclose(F3);
  exit (0);
}
